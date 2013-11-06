package hooker

import (
	"bytes"
	"fmt"
	"hash/crc64"
	"io"
	"io/ioutil"
	"net/http"
	"net/url"
	"time"

	"appengine"
	"appengine/memcache"
	"appengine/urlfetch"

	"github.com/dustin/go-jsonpointer"
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

var c64tab = crc64.MakeTable(crc64.ISO)

func hasSeen(c appengine.Context, dest, hashstr string) bool {
	h := crc64.New(c64tab)
	h.Write([]byte(dest))

	itm := &memcache.Item{
		Key:        fmt.Sprintf("seen-%v-%x", hashstr, h.Sum(nil)),
		Expiration: 2 * time.Hour,
	}

	return memcache.Add(c, itm) != nil
}

func deliverHook(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	var b io.Reader = r.Body
	if r.Header.Get("content-type") == "application/x-www-form-urlencoded" {
		bod, err := ioutil.ReadAll(io.LimitReader(r.Body, maxBody))
		if err != nil {
			panic(err)
		}
		form, err := url.ParseQuery(string(bod))
		if err != nil {
			panic(err)
		}
		b = bytes.NewReader(bod)

		j := []byte(form.Get("payload"))
		var hashstr string
		err = jsonpointer.FindDecode(j, "/after", &hashstr)
		if err != nil {
			err = jsonpointer.FindDecode(j, "/payload/head", &hashstr)
		}
		if err == nil {
			if hasSeen(c, r.Header.Get("x-dest"), hashstr) {
				c.Infof("Already processed %v -> %v",
					hashstr, r.Header.Get("x-dest"))
				return
			}
		} else {
			c.Infof("Can't find head, not trying to avoid it.")
		}
	}
	req, err := http.NewRequest(
		r.Header.Get("x-method"),
		r.Header.Get("x-dest"),
		ioutil.NopCloser(b))
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
	c.Infof("Set worker app stats to %.2f%%", appstats.RecordFraction*100.0)

	w.WriteHeader(204)
}

func stopHook(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(204)
}
