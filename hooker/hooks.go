package hooker

import (
	"net/http"
	"strings"
	"time"

	"appengine"
	"appengine/datastore"
	"appengine/user"
)

func init() {
	http.HandleFunc("/api/hooks/new", newHook)
	http.HandleFunc("/api/hooks/rm", deleteThing)
	http.HandleFunc("/api/hooks", listHooks)
}

func newHook(w http.ResponseWriter, r *http.Request) {
	c := appengine.NewContext(r)

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

	mustEncode(w, hook)
}

func listHooks(w http.ResponseWriter, r *http.Request) {
	c := appengine.NewContext(r)

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

func deleteThing(w http.ResponseWriter, r *http.Request) {
	c := appengine.NewContext(r)

	tid := r.FormValue("key")

	k, err := datastore.DecodeKey(tid)
	if err != nil {
		panic(err)
	}

	if err := datastore.Delete(c, k); err != nil {
		panic(err)
	}

	w.WriteHeader(204)
}
