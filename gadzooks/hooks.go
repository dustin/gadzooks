package gadzooks

import (
	"io"
	"io/ioutil"
	"net"
	"net/http"
	"strings"
	"sync"
	"time"

	"appengine"
	"appengine/datastore"
	"appengine/memcache"
	"appengine/taskqueue"

	"fmt"
	"github.com/mjibson/appstats"
)

const (
	maxBody        = 256 * 1024
	expirationTime = 86400 * time.Second
)

var githubBlock *net.IPNet

func init() {
	http.Handle("/deliver/", appstats.NewHandler(queueHook))

	_, inet, err := net.ParseCIDR("192.30.252.0/22")
	if err != nil {
		panic(err)
	}
	githubBlock = inet
}

func repoKey(repo string) string {
	return "repo-" + repo
}

func findHooks(c appengine.Context, repo string) ([]*Hook, error) {
	rv := []*Hook{}

	cacheKey := repoKey(repo)

	if item, err := memcache.JSON.Get(c, cacheKey, &rv); err == memcache.ErrCacheMiss {
		c.Infof("Reconstructing cache for %v", repo)
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
		c.Debugf("Found item in cache: %v", repo)
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

func queueHook(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	repo := r.URL.Path[len("/deliver/"):]

	if err := authenticateRepo(repo, r); err != nil {
		c.Warningf("Unauthorized trigger for %v: %v", repo, err)
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

	wg := sync.WaitGroup{}
	for _, hook := range hooks {
		wg.Add(1)
		go func(hook *Hook) {
			defer wg.Done()
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
			if err != nil {
				panic(err)
			}
		}(hook)
	}
	wg.Wait()

	w.WriteHeader(201)
}
