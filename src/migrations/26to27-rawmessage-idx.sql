CREATE INDEX CONCURRENTLY IF NOT EXISTS raw_message_scanned_for_urls ON raw_message(scanned_for_urls);
-- CREATE INDEX IF NOT EXISTS raw_message_scanned_for_urls ON raw_message(scanned_for_urls);