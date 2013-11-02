package hooker

import (
	"compress/gzip"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"time"

	"appengine"
	"appengine/datastore"
	"appengine/taskqueue"
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

func processFile(c appengine.Context, repos map[string]bool, fn string) error {
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

		repo := struct {
			Type       string
			Repository struct{ Owner, Name string }
			Payload    struct{ Head string }
		}{}
		err = json.Unmarshal([]byte(rm), &repo)
		if err != nil {
			c.Warningf("Error unmarshaling json from %s: %v", rm, err)
			continue
		}
		if repo.Type != "PushEvent" {
			continue
		}

		rname := repo.Repository.Owner + "/" + repo.Repository.Name
		if repos[rname] {
			form := url.Values{"payload": []string{string(rm)}}
			_, err = taskqueue.Add(c,
				taskqueue.NewPOSTTask("/deliver/"+rname, form), "")
			if err != nil {
				return err
			}
		}

		docs++
	}
	c.Infof("Found %v events in %v", docs, time.Since(start))
	return nil
}

func loadInterestingRepos(c appengine.Context) (map[string]bool, error) {
	q := datastore.NewQuery("Hook")
	found := map[string]bool{}
	for t := q.Run(c); ; {
		var x Hook
		_, err := t.Next(&x)
		if err == datastore.Done {
			break
		} else if err != nil {
			return nil, err
		}

		found[x.Repo] = true
	}
	return found, nil
}

func cronDownload(c appengine.Context, w http.ResponseWriter, r *http.Request) {

	repos, err := loadInterestingRepos(c)
	if err != nil {
		panic(err)
	}

	t, err := latestDownload(c)
	if err != nil {
		panic(err)
	}
	for _, t = range genDates(t, time.Now(), time.Hour) {
		err := processFile(c, repos, formatDate(t))
		if err != nil {
			c.Infof("Stopping at %v because %v", t, err)
			break
		}
		storeLatestDownload(c, t)
	}
	w.WriteHeader(204)
}
