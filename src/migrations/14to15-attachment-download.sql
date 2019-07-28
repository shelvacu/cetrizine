CREATE TABLE download_data (
  rowid                   serial8 not null primary key,
  sha256sum_hex           text not null,
  data                    bytea not null,
  downloaded_at           moment not null
);

CREATE TABLE download (
  rowid                   serial8 not null primary key,
  url                     text not null,
  response_code           int2,
  success                 bool not null,
  --attachment_rowid        int8 not null REFERENCES attachment(rowid),
  download_data_rowid     int8 REFERENCES download_data(rowid),
  downloaded_at           moment not null
);

CREATE INDEX download_url ON download(url);

ALTER TABLE attachment
  ADD COLUMN download_rowid int8 REFERENCES download(rowid);

CREATE INDEX attachment_download_rowid ON attachment(download_rowid,rowid);
CREATE INDEX download_data_sha256sum_hex ON download_data(sha256sum_hex);

--CREATE INDEX attachment_download_attachment_rowid ON attachment_download(attachment_rowid);



-----------------------
--cetrizine_shelvacu=# CREATE INDEX shelvacu_custom_idx1 ON attachment(message_rowid);
--CREATE INDEX
--cetrizine_shelvacu=# CREATE INDEX shelvacu_custom_idx2 ON message(channel_id,discord_id);
--CREATE INDEX
--cetrizine_shelvacu=# CREATE INDEX shelvacu_custom_idx3 ON message(discord_id);
