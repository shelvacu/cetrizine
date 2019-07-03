# TODO

- [X] Upon restart, don't re-download messages received live, keep track of ranges gotten so far (chance of missing messages so maybe make it configurable, but the assumption is that this program long-running so triggering a redownload should also be an option)
- [ ] Make sure all events that might indicate a channel can be archived that couldn't before are handled.
- [ ] Archive images and keep track of which are archived
- [ ] Merge newer version of serenity
- [ ] Migrate to Diesel for postgres access
