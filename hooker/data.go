package hooker

import (
	"fmt"
	"time"

	"appengine/datastore"
)

type Hook struct {
	Repo     string    `json:"repo"`
	Dest     string    `json:"dest"`
	Disabled bool      `json:"disabled"`
	Owner    string    `json:"owner"`
	Created  time.Time `json:"created"`
	Modified time.Time `json:"modified"`

	Key *datastore.Key `datastore:"-"`
}

func (h Hook) validate() error {
	if h.Repo == "" {
		return fmt.Errorf("invalid repo: %q", h.Repo)
	}
	if h.Dest == "" {
		return fmt.Errorf("invalid destination: %q", h.Dest)
	}
	if h.Owner == "" {
		return fmt.Errorf("invalid owner: %q", h.Owner)
	}
	return nil
}
