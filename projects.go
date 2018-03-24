package gadzooks

import (
	"net/http"
	"net/url"
	"os"
	"sort"
	"strings"
	"sync"
	"time"

	"golang.org/x/net/context"

	"google.golang.org/appengine/datastore"
	"google.golang.org/appengine/delay"
	"google.golang.org/appengine/log"
	"google.golang.org/appengine/memcache"
	"google.golang.org/appengine/user"

	"github.com/mjibson/appstats"
)

func init() {
	http.Handle("/api/projects/new", appstats.NewHandler(newProject))
	http.Handle("/api/projects/update", appstats.NewHandler(updateProject))
	http.Handle("/api/projects/rm", appstats.NewHandler(rmProject))
	http.Handle("/api/projects", appstats.NewHandler(lsProjects))

	http.Handle("/export/handlers", appstats.NewHandler(exportHandlers))
}

func syncHooks(c context.Context, pkey *datastore.Key) error {
	cacheKeys := []string{interestKey}
	err := datastore.RunInTransaction(c, func(tc context.Context) error {
		project := &Project{}
		err := datastore.Get(tc, pkey, project)
		if err != nil {
			return err
		}
		q := datastore.NewQuery("Hook").Ancestor(pkey).KeysOnly()
		keys, err := q.GetAll(tc, nil)
		if err != nil {
			return err
		}

		err = datastore.DeleteMulti(tc, keys)
		if err != nil {
			return err
		}

		keys = nil
		hooks := []*Hook{}
		for _, dep := range project.Deps {
			cacheKeys = append(cacheKeys, repoKey(dep))
			for _, hook := range project.Hooks {
				log.Infof(c, "Hooking %v -> %v", dep, hook)
				keys = append(keys, datastore.NewIncompleteKey(c, "Hook", pkey))
				hooks = append(hooks, &Hook{
					Project:  pkey,
					Repo:     dep,
					Dest:     hook,
					Owner:    project.Owner,
					Created:  project.Modified,
					Modified: project.Modified,
				})
			}
		}

		_, err = datastore.PutMulti(tc, keys, hooks)
		return err
	}, nil)

	if err == nil {
		log.Infof(c, "Clearing cache for %v", cacheKeys)
		memcache.DeleteMulti(c, cacheKeys)
	}
	return err

}

var syncHooksAsync = delay.Func("syncHooksAsync", syncHooks)

var deleteProject = delay.Func("deleteProject", func(c context.Context, pkey *datastore.Key) error {
	project := &Project{}
	if err := datastore.Get(c, pkey, project); err != nil {
		return err
	}

	q := datastore.NewQuery("Hook").Ancestor(pkey).KeysOnly()
	keys, err := q.GetAll(c, nil)
	if err != nil {
		return err
	}

	keys = append(keys, pkey)
	err = datastore.DeleteMulti(c, keys)

	if err == nil {
		cacheKeys := []string{interestKey}
		for _, r := range project.Deps {
			cacheKeys = append(cacheKeys, repoKey(r))
		}
		log.Infof(c, "Clearing cache for %v", cacheKeys)
		memcache.DeleteMulti(c, cacheKeys)
	}
	return err
})

func groupProjects(c context.Context, gkey *datastore.Key) ([]*datastore.Key, error) {
	q := datastore.NewQuery("Project").Filter("Group = ", gkey).KeysOnly()
	return q.GetAll(c, nil)
}

func generateKeys(c context.Context, q *datastore.Query,
	ch chan *datastore.Key, ech chan error, qch chan bool, wg *sync.WaitGroup) {
	defer wg.Done()

	keys, err := q.KeysOnly().GetAll(c, nil)
	if err != nil {
		select {
		case ech <- err:
		case <-qch:
			return
		}
		return
	}
	for _, k := range keys {
		select {
		case ch <- k:
		case <-qch:
			return
		}
	}
}

func waitForKeys(ch chan *datastore.Key, ech chan error, qch chan bool,
	wg *sync.WaitGroup) (map[string]*datastore.Key, error) {

	rv := map[string]*datastore.Key{}

	done := make(chan bool)
	go func() {
		wg.Wait()
		close(done)
	}()

	var err error

	for {
		select {
		case k := <-ch:
			rv[k.Encode()] = k
		case err = <-ech:
		case <-done:
			return rv, err
		}
	}
}

func exportHandlers(c context.Context, w http.ResponseWriter, r *http.Request) {
	if os.Getenv("AUTH_SECRET") != r.FormValue("auth") {
		log.Errorf(c, "%q != %q", os.Getenv("AUTH_SECRET"), r.FormValue("auth"))
		http.Error(w, "unauthorized", 403)
		return
	}

	repoU, err := loadInterestingReposURLs(c)
	if err != nil {
		log.Errorf(c, "error loading interesting repos: %v", err)
		http.Error(w, err.Error(), 500)
		return
	}
	var repos []string
	for k := range repoU {
		repos = append(repos, k)
	}
	sort.Strings(repos)
	mustEncode(w, repos)
}

func lsProjects(c context.Context, w http.ResponseWriter, r *http.Request) {
	ch := make(chan *datastore.Key)
	ech := make(chan error)
	qch := make(chan bool)
	wg := &sync.WaitGroup{}

	wg.Add(1)
	go generateKeys(c, datastore.NewQuery("Project").
		Filter("Owner = ", user.Current(c).Email),
		ch, ech, qch, wg)

	wg.Add(1)
	go func() {
		defer wg.Done()
		q := datastore.NewQuery("Group").Filter("Members =", user.Current(c).Email)
		keys, err := q.KeysOnly().GetAll(c, nil)
		if err != nil {
			select {
			case ech <- err:
			default:
			}
			return
		}
		wg2 := &sync.WaitGroup{}
		for _, k := range keys {
			wg2.Add(1)
			go generateKeys(c, datastore.NewQuery("Project").Filter("Group =", k),
				ch, ech, qch, wg2)
		}
		wg2.Wait()
	}()

	keysM, err := waitForKeys(ch, ech, qch, wg)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}

	var keys []*datastore.Key
	for _, k := range keysM {
		keys = append(keys, k)
	}

	results := make([]Project, len(keys))
	err = datastore.GetMulti(c, keys, results)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}

	for i := range keys {
		results[i].Key = keys[i]
	}

	mustEncode(w, results)
}

func maybeKey(s string) *datastore.Key {
	k, _ := datastore.DecodeKey(s)
	return k
}

func newProject(c context.Context, w http.ResponseWriter, r *http.Request) {
	t := time.Now().UTC()

	project := &Project{
		Owner:    user.Current(c).Email,
		Name:     strings.TrimSpace(r.FormValue("name")),
		Deps:     r.Form["deps"],
		Hooks:    r.Form["hooks"],
		Group:    maybeKey(r.FormValue("group")),
		Created:  t,
		Modified: t,
	}

	k, err := datastore.Put(c, datastore.NewIncompleteKey(c, "Project", nil), project)
	if err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	project.Key = k

	syncHooksAsync.Call(c, k)

	mustEncode(w, project)
}

var updateInner = delay.Func("updateInner", func(c context.Context, form url.Values) error {
	tid := form.Get("key")

	k, err := datastore.DecodeKey(tid)
	if err != nil {
		return err
	}

	project := &Project{}
	err = datastore.Get(c, k, project)
	if err != nil {
		return err
	}

	project.Modified = time.Now().UTC()
	project.Name = form.Get("name")
	project.Deps = form["deps"]
	project.Hooks = form["hooks"]
	project.Group = maybeKey(form.Get("group"))

	log.Infof(c, "Delayed update of %v for %v", project.Name, project.Owner)

	_, err = datastore.Put(c, k, project)
	if err != nil {
		return err
	}

	return syncHooks(c, k)
})

func updateProject(c context.Context, w http.ResponseWriter, r *http.Request) {
	r.ParseForm()
	updateInner.Call(c, r.Form)
	w.WriteHeader(202)
}

func rmProject(c context.Context, w http.ResponseWriter, r *http.Request) {
	k, err := datastore.DecodeKey(r.FormValue("key"))
	if err != nil {
		http.Error(w, "Can't decode key", 400)
		return
	}

	deleteProject.Call(c, k)

	w.WriteHeader(202)
}
