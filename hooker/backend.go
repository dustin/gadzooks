package hooker

import (
	"net/http"

	"appengine"
	"appengine/urlfetch"
)

func init() {
	http.HandleFunc("/backend/deliver", deliverHook)
}

func hookDeliveryError(c appengine.Context, r *http.Request, err error) {
	c.Errorf("Error delivering message relating to repo %v to %v for %v: %v",
		r.Header.Get("x-repo"), r.Header.Get("x-dest"),
		r.Header.Get("x-owner"), err)
}

func deliverHook(w http.ResponseWriter, r *http.Request) {
	c := appengine.NewContext(r)
	c.Infof("Handling backend req: %v", r.URL.String())

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
