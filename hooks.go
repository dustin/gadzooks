package gadzooks

import (
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net"
	"net/http"
	"strings"
	"time"

	"golang.org/x/net/context"
	"golang.org/x/sync/errgroup"

	"google.golang.org/appengine/datastore"
	"google.golang.org/appengine/log"
	"google.golang.org/appengine/memcache"
	"google.golang.org/appengine/taskqueue"

	"github.com/mjibson/appstats"
)

const (
	maxBody        = 256 * 1024
	expirationTime = 86400 * time.Second
)

var githubBlock *net.IPNet

func init() {
	http.Handle("/deliver/", appstats.NewHandler(queueHook))
	http.Handle("/queueHook/", appstats.NewHandler(queueHookExternal))
	http.Handle("/q/pull", appstats.NewHandler(hookTodo))
	http.Handle("/q/rm/", appstats.NewHandler(handleComplete))

	_, inet, err := net.ParseCIDR("192.30.252.0/22")
	if err != nil {
		panic(err)
	}
	githubBlock = inet
}

func repoKey(repo string) string {
	return "repo-" + repo
}

func findHooks(c context.Context, repo string) ([]*Hook, error) {
	rv := []*Hook{}

	cacheKey := repoKey(repo)

	if item, err := memcache.JSON.Get(c, cacheKey, &rv); err == memcache.ErrCacheMiss {
		log.Infof(c, "Reconstructing cache for %v", repo)
		q := datastore.NewQuery("Hook").Filter("Repo = ", repo)
		keys, err := q.GetAll(c, &rv)
		if err != nil {
			return nil, err
		}

		for i := range keys {
			rv[i].Key = keys[i]
		}

		item = &memcache.Item{
			Key:        cacheKey,
			Expiration: expirationTime,
			Object:     rv,
		}

		return rv, memcache.JSON.Set(c, item)
	} else if err != nil {
		return nil, err
	} else {
		log.Debugf(c, "Found item in cache: %v", repo)
	}

	return rv, nil
}

func isGithubRepo(repo string, r *http.Request) bool {
	return len(strings.Split(repo, "/")) == 2
}

func authenticateRepo(repo string, r *http.Request) error {
	if r.RemoteAddr == "0.1.0.2" {
		// task queue internal address
		return nil
	}
	if isGithubRepo(repo, r) {
		remote := net.ParseIP(r.RemoteAddr)
		if !githubBlock.Contains(remote) {
			return fmt.Errorf("Invalid github block: %v", remote)
		}
	}
	return nil
}

func queueHook(c context.Context, w http.ResponseWriter, r *http.Request) {
	repo := r.URL.Path[len("/deliver/"):]

	if err := authenticateRepo(repo, r); err != nil {
		log.Warningf(c, "Unauthorized trigger for %v: %v", repo, err)
		http.Error(w, err.Error(), 403)
		return
	}

	hooks, err := findHooks(c, repo)
	if err != nil {
		panic(err)
	}

	data, err := ioutil.ReadAll(io.LimitReader(r.Body, maxBody))
	if err != nil {
		panic(err)
	}

	g, _ := errgroup.WithContext(c)
	for _, hook := range hooks {
		hook := hook
		g.Go(func() error {
			task := &taskqueue.Task{
				Path:    "/backend/deliver",
				Payload: data,
				Header: http.Header{
					"x-method":     []string{r.Method},
					"x-repo":       []string{repo},
					"x-dest":       []string{hook.Dest},
					"x-owner":      []string{hook.Owner},
					"x-hook":       []string{hook.Key.Encode()},
					"content-type": []string{r.Header.Get("content-type")},
				},
			}

			_, err = taskqueue.Add(c, task, "deliver")
			return err
		})
	}
	if err := g.Wait(); err != nil {
		log.Errorf(c, "Error queuing hooks: %v", err)
		http.Error(w, err.Error(), 500)
		return
	}

	w.WriteHeader(201)
}

func queueHookExternal(c context.Context, w http.ResponseWriter, r *http.Request) {
	if err := checkAuth(r); err != nil {
		log.Errorf(c, "%v", err)
		http.Error(w, "unauthorized", 403)
		return
	}

	if err := r.ParseForm(); err != nil {
		log.Errorf(c, "error parsing form: %v", err)
		http.Error(w, err.Error(), 400)
		return
	}

	rname := r.URL.Path[len("/queueHook/"):]
	log.Debugf(c, "queueing hook for %q", rname)
	if _, err := taskqueue.Add(c, taskqueue.NewPOSTTask("/deliver/"+rname, r.Form), ""); err != nil {
		log.Errorf(c, "Error queueing task:  %v", err)
		http.Error(w, "can't queue task", 500)
		return
	}

	w.WriteHeader(204)
}

func reportError(c context.Context, w http.ResponseWriter, err error) {
	log.Warningf(c, "Error: %v", err)
	http.Error(w, "Error processing your request", 500)
}

func hookTodo(c context.Context, w http.ResponseWriter, r *http.Request) {
	if err := checkAuth(r); err != nil {
		log.Errorf(c, "%v", err)
		http.Error(w, "unauthorized", 403)
		return
	}

	tasks, err := taskqueue.LeaseByTag(c, 1, "todo", 1800, "")
	if err != nil {
		reportError(c, w, err)
		return
	}

	if len(tasks) != 1 {
		log.Infof(c, "No tasks found")
		w.WriteHeader(204)
		return
	}

	task := tasks[0]

	if err := json.NewEncoder(w).Encode(map[string]interface{}{
		"tid":  task.Name,
		"body": string(task.Payload),
	}); err != nil {
		reportError(c, w, err)
		return
	}
}

func handleComplete(c context.Context, w http.ResponseWriter, r *http.Request) {
	if err := checkAuth(r); err != nil {
		log.Errorf(c, "%v", err)
		http.Error(w, "unauthorized", 403)
		return
	}

	parts := strings.SplitN(r.URL.Path[6:], "/", 2)
	if len(parts) != 2 {
		http.Error(w, "you're doing it wrong", 400)
		return
	}

	if err := taskqueue.Delete(c, &taskqueue.Task{Name: parts[1]}, "todo"); err != nil {
		reportError(c, w, err)
		return
	}

	w.WriteHeader(204)
}
