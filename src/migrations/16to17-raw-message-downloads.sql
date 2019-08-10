ALTER TABLE raw_message
  ADD COLUMN scanned_for_urls bool not null default false;

CREATE TABLE raw_message_url (
  raw_message_rowid int8 not null REFERENCES raw_message(rowid),
  url text not null,
  been_downloaded bool not null,
  PRIMARY KEY (url, raw_message_rowid)
);

CREATE INDEX raw_message_url_been_downloaded on raw_message_url (been_downloaded, raw_message_rowid, url);