--TFW you miss a single "NOT" and don't notice for awhile because discord never sends binary websocket packets until now apparently

ALTER TABLE raw_message
  DROP CONSTRAINT raw_message_check;
CREATE INDEX IF NOT EXISTS raw_message_probably_valid ON raw_message(((kind = 'Text' AND content_text IS NOT NULL AND content_binary IS NULL)
    OR
    (kind in ('Binary', 'Ping', 'Pong') AND content_text IS NULL AND content_binary IS NOT NULL)
    OR
    (kind = 'Close' AND content_text IS NULL)));