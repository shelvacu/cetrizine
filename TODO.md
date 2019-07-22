# TODO

- [X] Upon restart, don't re-download messages received live, keep track of ranges gotten so far (chance of missing messages so maybe make it configurable, but the assumption is that this program long-running so triggering a redownload should also be an option)
- [X] Make sure all events that might indicate a channel can be archived that couldn't before are handled.
- [X] Merge newer version of serenity
- [X] Migrate to Diesel for postgres access
- [ ] Archive images and keep track of which are archived
    - [ ] Archive images directly attached to messages
