DROP FUNCTION IF EXISTS hdb_catalog.insert_event_log(text, text, text, json);

ALTER TABLE hdb_catalog.event_logs
DROP COLUMN paused boolean;
