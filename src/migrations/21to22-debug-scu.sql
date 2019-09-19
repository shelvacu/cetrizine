ALTER DOMAIN serenity_current_user
  DROP CONSTRAINT serenity_current_user_check;

-- CHECK(VALUE IS NULL OR(
--     (VALUE).inner_user IS NOT NULL AND
--     (VALUE).mfa_enabled IS NOT NULL AND
--     (VALUE).verified IS NOT NULL
--   ))

ALTER DOMAIN serenity_current_user
  ADD CONSTRAINT serenity_current_user_check_verified CHECK(VALUE IS NULL OR (VALUE).verified IS NOT NULL);
ALTER DOMAIN serenity_current_user
  ADD CONSTRAINT serenity_current_user_check_inner_user CHECK(VALUE IS NULL OR (VALUE).inner_user IS NOT NULL);
ALTER DOMAIN serenity_current_user
  ADD CONSTRAINT serenity_current_user_check_mfa_enabled CHECK(VALUE IS NULL OR (VALUE).mfa_enabled IS NOT NULL);
