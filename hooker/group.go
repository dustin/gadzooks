package hooker

import (
	"net/http"
	"net/url"

	"appengine"
	"appengine/datastore"
	"appengine/delay"
	"appengine/user"

	"github.com/mjibson/appstats"
)

func init() {
	http.Handle("/api/groups/new", appstats.NewHandler(newGroup))
	http.Handle("/api/groups/update", appstats.NewHandler(updateGroup))
	http.Handle("/api/groups/rm", appstats.NewHandler(rmGroup))
	http.Handle("/api/groups", appstats.NewHandler(lsGroups))
}

func userGroups(c appengine.Context, email string) ([]Group, error) {
	results := []Group{}

	q := datastore.NewQuery("Group").Filter("Members = ", email)
	keys, err := q.GetAll(c, &results)
	if err != nil {
		return nil, err
	}

	for i := range keys {
		results[i].Key = keys[i]
	}
	return results, nil
}

func lsGroups(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	results, err := userGroups(c, user.Current(c).Email)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	mustEncode(w, results)
}

func newGroup(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	group := &Group{Name: r.FormValue("name"), Members: []string{user.Current(c).Email}}
	k, err := datastore.Put(c, datastore.NewIncompleteKey(c, "Group", nil),
		group)

	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	group.Key = k

	mustEncode(w, group)
}

var updateGroupInner = delay.Func("updateGroupInner", func(c appengine.Context, form url.Values) error {
	k, err := datastore.DecodeKey(form.Get("key"))
	if err != nil {
		return err
	}

	group := &Group{}
	err = datastore.Get(c, k, group)
	if err != nil {
		return err
	}

	group.Name = form.Get("name")
	group.Members = form["members"]

	_, err = datastore.Put(c, k, group)
	return err
})

func updateGroup(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	r.ParseForm()
	updateGroupInner.Call(c, r.Form)
	w.WriteHeader(202)
}

var rmGroupInner = delay.Func("rmGroupInner", func(c appengine.Context, form url.Values) error {
	k, err := datastore.DecodeKey(form.Get("key"))
	if err != nil {
		return err
	}

	return datastore.Delete(c, k)
})

func rmGroup(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	r.ParseForm()
	rmGroupInner.Call(c, r.Form)
	w.WriteHeader(202)
}
