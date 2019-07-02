# TODO

* Make sure all events that might indicate a channel can be archived that couldn't before are handled.
* Archive images and keep track of which are archived
* Don't re-download messages received live upon restart, keep track of ranges gotten so far (chance of missing messages so maybe make it configurable, but the assumption is that this program long-running so triggering a redownload should also be an option)
* Merge newer version of serenity
* Migrate to Diesel for postgres access
