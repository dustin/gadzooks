package gadzooks

import (
	"time"

	"google.golang.org/appengine/datastore"
)

// Project contains all the data the user specifies for his/her
// project.
type Project struct {
	Owner    string         `json:"owner"`
	Name     string         `json:"name"`
	Deps     []string       `json:"deps"`
	Hooks    []string       `json:"hooks"`
	Created  time.Time      `json:"created"`
	Modified time.Time      `json:"modified"`
	Group    *datastore.Key `json:"Group,omitempty"`

	Key *datastore.Key `datastore:"-"`
}

// A Group is a list of users who wish to share ownership of a project.
type Group struct {
	Name    string   `json:"name"`
	Members []string `json:"members"`

	Key *datastore.Key `datastore:"-"`
}

// A Hook defines a webhook that will be triggered when a repostory
// event occurs.
type Hook struct {
	Project  *datastore.Key
	Repo     string    `json:"repo"`
	Dest     string    `json:"dest"`
	Disabled bool      `json:"disabled"`
	Owner    string    `json:"owner"`
	Created  time.Time `json:"created"`
	Modified time.Time `json:"modified"`

	Key *datastore.Key `datastore:"-"`
}
