package hooker

import (
	"compress/gzip"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"time"

	"appengine"
	"appengine/datastore"
	"appengine/urlfetch"

	"github.com/mjibson/appstats"
)

const (
	dlOldestStr = "2013-11-02T00"
	archiveu    = "http://data.githubarchive.org/"
)

var dlOldest = mustParseTs("2006-01-02T04", dlOldestStr)

func init() {
	http.Handle("/cron/download", appstats.NewHandler(cronDownload))
}

func mustParseTs(f, s string) time.Time {
	t, err := time.Parse(f, s)
	if err != nil {
		panic(err)
	}
	return t
}

type dlState struct {
	Latest time.Time
}

func genDates(from, to time.Time, by time.Duration) []time.Time {
	result := []time.Time{}
	for t := from; t.Before(to); t = t.Add(by) {
		result = append(result, t)
	}
	return result
}

func formatDate(t time.Time) string {
	return fmt.Sprintf("%04d-%02d-%02d-%d",
		t.Year(), t.Month(), t.Day(), t.Hour())
}

func latestDownload(c appengine.Context) (time.Time, error) {
	k := datastore.NewKey(c, "DLState", "dlstate", 0, nil)
	d := dlState{}
	err := datastore.Get(c, k, &d)
	if err == datastore.ErrNoSuchEntity {
		err = nil
		d.Latest = dlOldest
	}
	return d.Latest, err
}

func storeLatestDownload(c appengine.Context, t time.Time) error {
	_, err := datastore.Put(c,
		datastore.NewKey(c, "DLState", "dlstate", 0, nil), &dlState{t})
	return err
}

func processFile(c appengine.Context, fn string) error {
	c.Infof("Downloading %v", fn)
	start := time.Now()
	h := urlfetch.Client(c)
	res, err := h.Get(archiveu + fn + ".json.gz")
	if err != nil {
		return err
	}
	if res.StatusCode != 200 {
		return fmt.Errorf("Error grabbing %v: %v", fn, res)
	}
	defer res.Body.Close()
	z, err := gzip.NewReader(res.Body)
	if err != nil {
		return err
	}
	d := json.NewDecoder(z)
	docs := 0
	for {
		rm := json.RawMessage{}
		err := d.Decode(&rm)
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}
		docs++
	}
	c.Infof("Found %v events in %v", docs, time.Since(start))
	return nil
}

func cronDownload(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	t, err := latestDownload(c)
	if err != nil {
		panic(err)
	}
	for _, t = range genDates(t, time.Now(), time.Hour) {
		err := processFile(c, formatDate(t))
		if err != nil {
			c.Infof("Stopping at %v because %v", t, err)
			break
		}
	}
	storeLatestDownload(c, t)
	w.WriteHeader(204)
}
