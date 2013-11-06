package hooker

import (
	"io"
	"io/ioutil"
	"net/http"
	"sync"
	"time"

	"appengine"
	"appengine/datastore"
	"appengine/memcache"
	"appengine/taskqueue"

	"github.com/mjibson/appstats"
)

const (
	maxBody        = 256 * 1024
	expirationTime = 86400 * time.Second
)

func init() {
	http.Handle("/deliver/", appstats.NewHandler(queueHook))
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
