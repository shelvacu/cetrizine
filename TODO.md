# TODO

- [X] Upon restart, don't re-download messages received live, keep track of ranges gotten so far (chance of missing messages so maybe make it configurable, but the assumption is that this program long-running so triggering a redownload should also be an option)
- [X] Make sure all events that might indicate a channel can be archived that couldn't before are handled.
- [X] Merge newer version of serenity
- [X] Migrate to Diesel for postgres access
- [X] Archive images and keep track of which are archived
    - [X] Archive images directly attached to messages
    - [X] Archive all images (user images, guild images, embeds, etc)
    - [X] Archive all urls in any metadata
- [ ] Archive server audit logs (they only last 90 days! D:)
- [ ] Archive channel invites
- [ ] Archive channel webhooks
