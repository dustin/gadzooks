package hooker

import (
	"net/http"
	"net/url"
	"strings"
	"time"

	"appengine"
	"appengine/datastore"
	"appengine/delay"
	"appengine/memcache"
	"appengine/user"

	"github.com/mjibson/appstats"
)

func init() {
	http.Handle("/api/projects/new", appstats.NewHandler(newProject))
	http.Handle("/api/projects/update", appstats.NewHandler(updateProject))
	http.Handle("/api/projects/rm", appstats.NewHandler(rmProject))
	http.Handle("/api/projects", appstats.NewHandler(lsProjects))
}

var syncHooks = delay.Func("syncHooks", func(c appengine.Context, pkey *datastore.Key) error {
	cacheKeys := []string{interestKey}
	err := datastore.RunInTransaction(c, func(tc appengine.Context) error {
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
				tc.Infof("Hooking %v -> %v", dep, hook)
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
		c.Infof("Clearing cache for %v", cacheKeys)
		memcache.DeleteMulti(c, cacheKeys)
	}
	return err

})

var deleteProject = delay.Func("deleteProject", func(c appengine.Context, pkey *datastore.Key) error {
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
		c.Infof("Clearing cache for %v", cacheKeys)
		memcache.DeleteMulti(c, cacheKeys)
	}
	return err
})

func lsProjects(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	results := []Project{}

	q := datastore.NewQuery("Project").
		Filter("Owner = ", user.Current(c).Email).
		Order("Name")

	for t := q.Run(c); ; {
		var x Project
		k, err := t.Next(&x)
		if err != nil {
			break
		}
		x.Key = k
		results = append(results, x)
	}

	mustEncode(w, results)
}

func newProject(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	t := time.Now().UTC()

	project := &Project{
		Owner:    user.Current(c).Email,
		Name:     strings.TrimSpace(r.FormValue("name")),
		Deps:     r.Form["deps"],
		Hooks:    r.Form["hooks"],
		Created:  t,
		Modified: t,
	}

	k, err := datastore.Put(c, datastore.NewIncompleteKey(c, "Project", nil), project)
	if err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	project.Key = k

	syncHooks.Call(c, k)

	mustEncode(w, project)
}

var updateInner = delay.Func("updateInner", func(c appengine.Context, form url.Values) error {
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

	c.Infof("Delayed update of %v for %v", project.Name, project.Owner)

	_, err = datastore.Put(c, k, project)
	if err != nil {
		return err
	}

	syncHooks.Call(c, k)
	return nil
})

func updateProject(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	r.ParseForm()
	updateInner.Call(c, r.Form)
	w.WriteHeader(202)
}

func rmProject(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	k, err := datastore.DecodeKey(r.FormValue("key"))
	if err != nil {
		http.Error(w, "Can't decode key", 400)
		return
	}

	deleteProject.Call(c, k)

	w.WriteHeader(202)
}
