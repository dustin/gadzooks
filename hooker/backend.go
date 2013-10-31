package hooker

import (
	"net/http"

	"appengine"
	"appengine/urlfetch"

	"github.com/mjibson/appstats"
)

func init() {
	http.Handle("/backend/deliver", appstats.NewHandler(deliverHook))

	http.HandleFunc("/_ah/start", startHook)
	http.HandleFunc("/_ah/stop", stopHook)
}

func hookDeliveryError(c appengine.Context, r *http.Request, err error) {
	c.Errorf("Error delivering message relating to repo %v to %v for %v: %v",
		r.Header.Get("x-repo"), r.Header.Get("x-dest"),
		r.Header.Get("x-owner"), err)
}

func deliverHook(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	req, err := http.NewRequest(
		r.Header.Get("x-method"),
		r.Header.Get("x-dest"),
		r.Body)
	if err != nil {
		hookDeliveryError(c, r, err)
		return
	}
	req.Header.Set("content-type", r.Header.Get("content-type"))

	client := urlfetch.Client(c)
	res, err := client.Do(req)
	if err != nil {
		hookDeliveryError(c, r, err)
		return
	}
	defer res.Body.Close()

	c.Infof("Delivered message relating to repo %v to %v for %v: %v",
		r.Header.Get("x-repo"), r.Header.Get("x-dest"),
		r.Header.Get("x-owner"), res.Status)

	w.WriteHeader(res.StatusCode)
}

func startHook(w http.ResponseWriter, r *http.Request) {
	c := appengine.NewContext(r)

	appstats.RecordFraction = 0.05
	c.Infof("Set worker app stats to %2f%%", appstats.RecordFraction*100.0)

	w.WriteHeader(204)
}

func stopHook(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(204)
}
