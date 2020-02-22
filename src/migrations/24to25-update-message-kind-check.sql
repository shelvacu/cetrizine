-- ... MemberJoin, NitroBoost, NitroTier1, NitroTier2, NitroTier3

-- Old idea: Data constraints everywhere! Make sure everything is consistent! Yay consistent databases!
-- Thing happened: `ERROR:  new row for relation "message" violates check constraint "message_kind_check"` ie messages weren't saved, because I dun goofed.
-- New idea: Create an index that verifies if a row is "valid". Let things be inserted.

ALTER TABLE message
  DROP CONSTRAINT message_kind_check;

ALTER TABLE guild_channel
  DROP CONSTRAINT guild_channel_kind_check;

-- UPDATE message SET kind = 'NitroBoost' WHERE kind = 'UserPremiumGuildSubscription';
-- UPDATE message SET kind = 'NitroBoostTier1' WHERE kind = 'UserPremiumGuildSubscriptionTier1';
-- UPDATE message SET kind = 'NitroBoostTier2' WHERE kind = 'UserPremiumGuildSubscriptionTier2';
-- UPDATE message SET kind = 'NitroBoostTier3' WHERE kind = 'UserPremiumGuildSubscriptionTier3';

UPDATE message SET kind = 
  CASE
  WHEN kind = 'UserPremiumGuildSubscription' THEN 'NitroBoost'
  WHEN kind = 'UserPremiumGuildSubscriptionTier1' THEN 'NitroBoostTier1'
  WHEN kind = 'UserPremiumGuildSubscriptionTier2' THEN 'NitroBoostTier2'
  WHEN kind = 'UserPremiumGuildSubscriptionTier3' THEN 'NitroBoostTier3'
  ELSE kind
  END
  WHERE (kind IN ('UserPremiumGuildSubscription','UserPremiumGuildSubscriptionTier1','UserPremiumGuildSubscriptionTier2','UserPremiumGuildSubscriptionTier3'))
  ;

-- ALTER TABLE message
--   ADD CONSTRAINT message_kind_check CHECK (kind in ('Regular','GroupRecipientAddition','GroupRecipientRemoval','GroupCallCreation','GroupNameUpdate','GroupIconUpdate','PinsAdd','MemberJoin', 'UserPremiumGuildSubscription', 'UserPremiumGuildSubscriptionTier1', 'UserPremiumGuildSubscriptionTier2', 'UserPremiumGuildSubscriptionTier3'))
--   ;



CREATE INDEX CONCURRENTLY message_kind_check ON message ((kind IN ('Regular','GroupRecipientAddition','GroupRecipientRemoval','GroupCallCreation','GroupNameUpdate','GroupIconUpdate','PinsAdd','MemberJoin', 'NitroBoost', 'NitroBoostTier1', 'NitroBoostTier2', 'NitroBoostTier3')));

CREATE INDEX CONCURRENTLY guild_channel_kind_check ON guild_channel ((kind IN ('Text','Private','Voice','Group','Category','News','Store')));