-- Instead of just storing all this data, I want to actually be querying and indexing it. However, instead of trying to doing the strategy of a table for every type of discord object and then ALTERing TABLEs when discord updates, instead I'd like to take the data directly from the raw_message table with a zillion indexes. The first step of this is to make the raw_messages table store the data as postgres's json type as opposed to raw text or even the hypothetical binary. However, I still want to store the text if the JSON is invalid.

-- I plan to do this in a few steps, to hopefully make for minimal downtime:
-- 1. Add a column for json-typed data, default null
-- 2. Edit the code to properly fill that column as messages arrive.
-- 3. Start an SQL job to "fill in" legacy rows