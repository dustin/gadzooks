Webhook hooker.

The goal of this project is to make it easier for people to hook
webhooks together without the transmitting party having to configure
up all the endpoints users may want.

There's still a lot of work to do to make this *great*, but I'm
currently using it to trigger builds on go projects when upstream
software changes.

# Github Hooks

Currently, any changes to public github projects (anyone's) are
automatically and immediately triggered.

To use this, login to [the public instance][instance] and create a
project.  This project represents *your* project.  You can list the
dependencies as `deps` and have outbound URLs that will receive a copy
of any hook for a given dependency.

You can use this, for example, to run a build of your project on
[drone.io](http://drone.io/) any time an upstream dependency changes
so you can know quickly when those changes break your project.

For example, if you depend on
[go-humanize](https://github.com/dustin/go-humanize), you can add
`dustin/go-humanize` as a dep on your project.

[instance]: http://coastal-volt-254.appspot.com/
