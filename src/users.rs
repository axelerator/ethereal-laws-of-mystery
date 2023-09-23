extern crate argon2;

use argon2::{
    password_hash::{rand_core::OsRng, PasswordHasher, SaltString},
    Argon2,
};
use rusqlite::{params, Connection};
use serde::{Deserialize, Serialize};
use serde_rusqlite::*;
use std::collections::HashMap;
use tokio::sync::MutexGuard;
use tracing::debug;
use webauthn_rs::prelude::AuthenticationResult;
use webauthn_rs::prelude::Passkey;
use webauthn_rs::prelude::Uuid;

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone, Hash)]
pub struct SessionId {
    id: Uuid,
}

impl SessionId {
    pub fn new() -> SessionId {
        SessionId { id: Uuid::new_v4() }
    }
}

pub type UserId = Uuid;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct User {
    pub id: Uuid,
    pub name: String,
}

pub struct Users {
    pub name_to_id: HashMap<String, Uuid>,
    pub keys: HashMap<Uuid, Vec<Passkey>>,
    pub salt: SaltString,
}

pub struct PersistedPasskey {
    pub id: i64,
    pub passkey: Passkey,
}

impl Users {
    pub fn new() -> Users {
        let salt = SaltString::generate(&mut OsRng);
        Users {
            name_to_id: HashMap::new(),
            keys: HashMap::new(),
            salt,
        }
    }

    pub fn register(
        &mut self,
        user_unique_id: UserId,
        sk: &Passkey,
        username: String,
        connection: &MutexGuard<Connection>,
    ) {
        self.name_to_id.insert(username.clone(), user_unique_id);
        let passkey_toml: String = toml::to_string(&sk).expect("Failed to serialize PassKey");
        let user = self
            .by_username(&username, connection)
            .or_else(|| {
                let user = User {
                    id: user_unique_id,
                    name: username,
                };

                connection
                    .execute(
                        "INSERT INTO users (id, name) VALUES (:id, :name)",
                        to_params_named(&user).unwrap().to_slice().as_slice(),
                    )
                    .expect("Failed to insert");
                Some(user)
            })
            .unwrap();

        connection
            .execute(
                "INSERT INTO credentials (user_id, type, payload) VALUES (?1, ?2, ?3)",
                (&user.id.to_string(), "passkey", &passkey_toml),
            )
            .unwrap();
    }

    pub fn by_username(&self, username: &str, connection: &MutexGuard<Connection>) -> Option<User> {
        let mut statement = connection
            .prepare("SELECT * FROM users WHERE name = :username")
            .unwrap();

        let mut res = statement
            .query_and_then(&[(":username", username)], from_row::<User>)
            .unwrap();

        res.next().map(|row| row.unwrap())
    }

    pub fn by_username_and_password(
        &self,
        username: &str,
        password: &str,
        connection: &MutexGuard<Connection>,
    ) -> Option<User> {
        let argon2 = Argon2::default();
        let hashed_password = argon2
            .hash_password(password.as_bytes(), &self.salt)
            .unwrap()
            .to_string();
        let query = r#"
                SELECT * FROM users 
                INNER JOIN credentials on credentials.user_id = users.id 
                WHERE name = :username AND payload = :payload
            "#;
        let mut statement = connection.prepare(query).unwrap();

        let mut res = statement
            .query_and_then(
                &[(":username", username), (":payload", &hashed_password)],
                from_row::<User>,
            )
            .unwrap();

        res.next().map(|row| row.unwrap())
    }

    pub fn passkey_credentials_for(
        &self,
        user_id: &UserId,
        connection: &MutexGuard<Connection>,
    ) -> Vec<PersistedPasskey> {
        let mut stmt = connection
            .prepare("SELECT ROWID, payload FROM credentials WHERE user_id = :user_id AND type = 'passkey'")
            .unwrap();
        let user_id = user_id.to_string();
        let params = [(":user_id", user_id.as_str())];
        stmt.query_map(&params, |row| {
            let id = row.get(0).unwrap();
            let toml_string: String = row.get(1).unwrap();
            let passkey: Passkey =
                toml::from_str(&toml_string).expect("Failed to deserialize PassKey");
            let ppk = PersistedPasskey { id, passkey };
            Ok(ppk)
        })
        .unwrap()
        .map(|p| p.unwrap())
        .collect()
    }

    pub fn update_credentials(
        &self,
        credentials: &mut Vec<PersistedPasskey>,
        auth_result: &AuthenticationResult,
        connection: &MutexGuard<Connection>,
    ) {
        // Update the credential counter, if possible.
        credentials.iter_mut().for_each(|ppk| {
            // This will update the credential if it's the matching
            // one. Otherwise it's ignored. That is why it is safe to
            // iterate this over the full list.
            ppk.passkey.update_credential(auth_result);
            let passkey_toml: String =
                toml::to_string(&ppk.passkey).expect("Failed to serialize PassKey");
            connection
                .execute(
                    "UPDATE credentials SET payload = ?1 WHERE ROWID = ?2",
                    params![&passkey_toml, &ppk.id],
                )
                .unwrap();
        });
    }

    pub fn register_with_credentials(
        &self,
        username: &str,
        password: &str,
        connection: &MutexGuard<Connection>,
    ) -> std::result::Result<User, String> {
        let user = self.by_username(&username, connection);

        match user {
            Some(_) => Err("Username already taken".to_string()),
            None => {
                let id = Uuid::new_v4();
                let user = User {
                    id,
                    name: username.to_string(),
                };
                connection
                    .execute(
                        "INSERT INTO users (id, name) VALUES (:id, :name)",
                        to_params_named(&user).unwrap().to_slice().as_slice(),
                    )
                    .expect("Failed to insert");

                let argon2 = Argon2::default();
                let hashed_password = argon2
                    .hash_password(password.as_bytes(), &self.salt)
                    .unwrap()
                    .to_string();

                debug!("INSERT INTO credentials (user_id, type, payload) VALUES (?1, ?2, ?3)");
                connection
                    .execute(
                        "INSERT INTO credentials (user_id, type, payload) VALUES (?1, ?2, ?3)",
                        (&user.id.to_string(), "password", &hashed_password),
                    )
                    .unwrap();

                Ok(user)
            }
        }
    }
}
