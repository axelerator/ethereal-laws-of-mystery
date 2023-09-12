CREATE TABLE users (id TEXT PRIMARY KEY, name TEXT);
CREATE TABLE credentials (user_id TEXT, passkey_toml TEXT);
