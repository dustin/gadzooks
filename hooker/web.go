package hooker

import (
	"encoding/json"
	"io"
	"io/ioutil"
	"net/http"

	"appengine"
	"appengine/user"
)

var appStatic []byte

func init() {
	var err error

	appStatic, err = ioutil.ReadFile("templates/app.html")
	if err != nil {
		panic(err)
	}

	http.HandleFunc("/", serveHome)
	http.HandleFunc("/app/", serveAppStatic)
	http.HandleFunc("/api/currentuser/", currentUser)
	http.HandleFunc("/logout", logoutRedirect)
}

func mustEncode(w io.Writer, i interface{}) {
	if headered, ok := w.(http.ResponseWriter); ok {
		headered.Header().Set("Cache-Control", "no-cache")
		headered.Header().Set("Content-type", "application/json")
	}

	e := json.NewEncoder(w)
	if err := e.Encode(i); err != nil {
		panic(err)
	}
}

func serveAppStatic(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/html")
	w.Write(appStatic)
}

func logoutRedirect(w http.ResponseWriter, r *http.Request) {
	url, _ := user.LogoutURL(appengine.NewContext(r), "/")
	http.Redirect(w, r, url, http.StatusTemporaryRedirect)
}

func currentUser(w http.ResponseWriter, r *http.Request) {
	c := appengine.NewContext(r)
	mustEncode(w, user.Current(c))
}

func serveHome(w http.ResponseWriter, r *http.Request) {
	http.Redirect(w, r, "/app/", http.StatusFound)
}
