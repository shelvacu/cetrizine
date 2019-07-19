ALTER TABLE migration_version
  DROP COLUMN enforce_single_row,
  ADD COLUMN enforce_single_row bool PRIMARY KEY DEFAULT true CHECK(enforce_single_row);
