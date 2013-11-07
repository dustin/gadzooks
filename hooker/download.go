package hooker

import (
	"compress/gzip"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"sync"
	"time"

	"appengine"
	"appengine/datastore"
	"appengine/memcache"
	"appengine/taskqueue"
	"appengine/urlfetch"

	"github.com/mjibson/appstats"
)

const (
	archiveu    = "http://data.githubarchive.org/"
	interestKey = "interesting"
)

func init() {
	http.Handle("/cron/download", appstats.NewHandler(cronDownload))
	http.Handle("/backend/ghadownload", appstats.NewHandler(ghaDownload))
	http.Handle("/public/interesting", appstats.NewHandler(interesting))
}

func mustParseTs(f, s string) time.Time {
	t, err := time.Parse(f, s)
	if err != nil {
		panic(err)
	}
	return t
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

func eventProcessor(c appengine.Context, repos map[string]int,
	ch chan []byte, wg *sync.WaitGroup) {
	defer wg.Done()

	for data := range ch {
		repo := struct {
			Type       string
			Repository struct{ Owner, Name string }
			Payload    struct{ Head string }
		}{}
		err := json.Unmarshal(data, &repo)
		if err != nil {
			c.Warningf("Error unmarshaling json from %s: %v", data, err)
			continue
		}
		if repo.Type != "PushEvent" {
			continue
		}

		rname := repo.Repository.Owner + "/" + repo.Repository.Name
		if repos[rname] > 0 {
			form := url.Values{"payload": []string{string(data)}}
			_, err = taskqueue.Add(c,
				taskqueue.NewPOSTTask("/deliver/"+rname, form), "")
			if err != nil {
				c.Errorf("Error queueing task:  %v", err)
			}
		}
	}
}

func processFile(c appengine.Context, repos map[string]int, fn string) error {
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

	wg := &sync.WaitGroup{}
	ch := make(chan []byte)
	for i := 0; i < 8; i++ {
		wg.Add(1)
		go eventProcessor(c, repos, ch, wg)
	}

	docs := 0
	for {
		rm := json.RawMessage{}
		err := d.Decode(&rm)
		if err == io.EOF {
			break
		}
		if err != nil {
			close(ch)
			return err
		}

		ch <- []byte(rm)

		docs++
	}
	close(ch)
	wg.Wait()
	c.Infof("Found %v events in %v", docs, time.Since(start))
	return nil
}

func loadInterestingRepos(c appengine.Context) (map[string]int, error) {
	found := map[string]int{}
	if item, err := memcache.JSON.Get(c, interestKey, &found); err == memcache.ErrCacheMiss {
		q := datastore.NewQuery("Hook")
		for t := q.Run(c); ; {
			var x Hook
			_, err := t.Next(&x)
			if err == datastore.Done {
				break
			} else if err != nil {
				return nil, err
			}

			found[x.Repo]++
		}
		item = &memcache.Item{
			Key:        interestKey,
			Object:     found,
			Expiration: expirationTime,
		}
		return found, memcache.JSON.Set(c, item)
	}
	return found, nil
}

func ghaDownload(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	repos, err := loadInterestingRepos(c)
	if err != nil {
		c.Errorf("Error loading repos: %v", err)
		http.Error(w, "Error loading repos:  "+err.Error(), 500)
		return
	}
	err = processFile(c, repos, r.FormValue("fn"))
	if err != nil {
		c.Errorf("Error processing file: %v", err)
		http.Error(w, "Error processing file: "+err.Error(), 503)
		return
	}
	w.WriteHeader(204)
}

func cronDownload(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	loc, err := time.LoadLocation("US/Pacific")
	if err != nil {
		http.Error(w, "Can't find timezone: "+err.Error(), 500)
		return
	}
	t := time.Now().Add(time.Hour * -1).In(loc)

	taskqueue.Add(c, taskqueue.NewPOSTTask("/backend/ghadownload",
		url.Values{"fn": []string{formatDate(t)}}), "gharchive")

	w.WriteHeader(202)
}

func interesting(c appengine.Context, w http.ResponseWriter, r *http.Request) {
	i, err := loadInterestingRepos(c)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}

	mustEncode(w, i)
}
