package hooker

import (
	"io"
	"io/ioutil"
	"net/http"
	"strings"
	"sync"
	"time"

	"appengine"
	"appengine/datastore"
	"appengine/memcache"
	"appengine/taskqueue"
	"appengine/user"

	"github.com/mjibson/appstats"
)

const maxBody = 64 * 1024

func init() {
	http.Handle("/api/hooks/new", appstats.NewHandler(newHook))
	http.Handle("/api/hooks/rm", appstats.NewHandler(deleteHook))
	http.Handle("/api/hooks", appstats.NewHandler(listHooks))
	http.Handle("/deliver/", appstats.NewHandler(queueHook))
}

func newHook(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	t := time.Now().UTC()

	hook := &Hook{
		Repo:     strings.TrimSpace(r.FormValue("repo")),
		Dest:     strings.TrimSpace(r.FormValue("dest")),
		Owner:    user.Current(c).Email,
		Created:  t,
		Modified: t,
	}

	if err := hook.validate(); err != nil {
		http.Error(w, err.Error(), 400)
		return
	}

	k, err := datastore.Put(c, datastore.NewIncompleteKey(c, "Hook", nil), hook)
	if err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	hook.Key = k

	memcache.Delete(c, repoKey(hook.Repo))

	mustEncode(w, hook)
}

func listHooks(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	results := []Hook{}

	q := datastore.NewQuery("Hook").
		Filter("Owner = ", user.Current(c).Email).
		Order("Repo")

	for t := q.Run(c); ; {
		var x Hook
		k, err := t.Next(&x)
		if err != nil {
			break
		}
		x.Key = k
		results = append(results, x)
	}

	mustEncode(w, results)
}

func deleteHook(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	tid := r.FormValue("key")

	k, err := datastore.DecodeKey(tid)
	if err != nil {
		panic(err)
	}

	memcache.Delete(c, repoKey(r.FormValue("repo")))

	if err := datastore.Delete(c, k); err != nil {
		panic(err)
	}

	w.WriteHeader(204)
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
		for t := q.Run(c); ; {
			var x Hook
			k, err := t.Next(&x)
			if err != nil {
				break
			}
			x.Key = k

			rv = append(rv, &x)
		}

		item = &memcache.Item{
			Key:        cacheKey,
			Expiration: 3600 * time.Second,
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

func queueHook(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	repo := r.URL.Path[len("/deliver/"):]
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
		go func() {
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
		}()
	}
	wg.Wait()

	w.WriteHeader(201)
}
