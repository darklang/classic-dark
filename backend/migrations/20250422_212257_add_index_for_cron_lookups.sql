--#[no_tx]

-- this index makes the 'get all active crons' a much more performant query
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_toplevel_oplists_filter
ON toplevel_oplists (module, modifier, name, deleted)