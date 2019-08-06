ALTER TABLE raw_message
  ADD COLUMN downloaded_any_files bool not null default false;

CREATE TABLE raw_message_to_download (
  raw_message_rowid int8 not null REFERENCES raw_message(rowid),
  download_rowid int8 not null REFERENCES download(rowid),
  PRIMARY KEY (raw_message_rowid, download_rowid) --diesel *requires* a primary key
);

--CREATE INDEX rmtd_rm_d ON raw_message_to_download(raw_message_rowid, download_rowid);
CREATE UNIQUE INDEX rmtd_d_rm ON raw_message_to_download(download_rowid, raw_message_rowid);
