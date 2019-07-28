CREATE TABLE download_header (
  rowid                   serial8 not null primary key,
  download_rowid          int8 not null REFERENCES download(rowid),
  header_name             text not null,
  header_value            bytea not null
);

CREATE INDEX download_header_download_rowid_header_name ON download_header(download_rowid, header_name);
ALTER TABLE download
  ADD COLUMN has_headers bool not null default false;
ALTER TABLE download
  ADD COLUMN content_type text default null;
