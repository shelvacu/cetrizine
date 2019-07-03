ALTER TABLE message
  --ADD COLUMN prior_message_received int8 REFERENCES message(rowid) , --possibly null
  DROP CONSTRAINT message_kind_check ,
  ADD CONSTRAINT message_kind_check CHECK (kind in ('Regular','GroupRecipientAddition','GroupRecipientRemoval','GroupCallCreation','GroupNameUpdate','GroupIconUpdate','PinsAdd','MemberJoin', 'UserPremiumGuildSubscription', 'UserPremiumGuildSubscriptionTier1', 'UserPremiumGuildSubscriptionTier2', 'UserPremiumGuildSubscriptionTier3'))
  ;
ALTER TABLE message_archive_gets
  ADD COLUMN synthetic bool NOT null DEFAULT false ,
  ADD COLUMN finished bool NOT null DEFAULT true ,
  ADD COLUMN session_id int8 REFERENCES run_session(rowid) DEFAULT null ,
  ALTER COLUMN message_count_requested DROP NOT NULL ,
  ALTER COLUMN message_count_received DROP NOT NULL ,
  ALTER COLUMN start_message_id DROP NOT NULL ,
  ALTER COLUMN end_message_id DROP NOT NULL ,
  ADD CONSTRAINT message_archive_gets_count_check CHECK(
    synthetic OR (message_count_requested IS NOT NULL AND message_count_received IS NOT NULL)
  ) ,
  ADD CONSTRAINT message_archive_gets_finished_check CHECK(
    finished OR (synthetic AND NOT finished)
  ) ,
  ADD CONSTRAINT message_archive_gets_message_ids_check CHECK(
    ( start_message_id IS NOT NULL AND end_message_id IS NOT NULL ) OR NOT finished
  ) ,
  DROP CONSTRAINT message_archive_gets_check ,
  ADD CONSTRAINT message_archive_gets_check CHECK(
    (
      after_message_id IS NULL
     AND
      around_message_id IS NULL
     AND
      before_message_id IS NOT NULL
    )
    OR
    (
      after_message_id IS NULL
     AND
      around_message_id IS NOT NULL
     AND
      before_message_id IS NULL
    )
    OR
    (
      after_message_id IS NOT NULL
     AND
      around_message_id IS NULL
     AND
      before_message_id IS NULL
    )
    OR
    (
      ( legacy OR synthetic )
     AND
      after_message_id IS NULL
     AND
      around_message_id IS NULL
     AND
      before_message_id IS NULL
    )
  )
  ;
CREATE UNIQUE INDEX mag_synthetic ON message_archive_gets (session_id, channel_id) WHERE (synthetic);
