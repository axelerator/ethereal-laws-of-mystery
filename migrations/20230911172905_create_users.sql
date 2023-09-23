CREATE TABLE users (id TEXT PRIMARY KEY, name TEXT);
CREATE TABLE credentials (user_id TEXT, type TEXT, payload TEXT);
