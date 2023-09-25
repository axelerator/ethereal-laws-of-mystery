use std::collections::HashSet;

use crate::hades::RealmId;
use crate::startup::AppState;
use crate::{error::WebauthnError, users::SessionId};
use axum::{
    extract::{Extension, Json, Path},
    http::StatusCode,
    response::IntoResponse,
};
use axum_sessions::extractors::WritableSession;
use elm_rs::{Elm, ElmDecode, ElmEncode};
use serde::{Deserialize, Serialize};

/*
 * Webauthn RS auth handlers.
 * These files use webauthn to process the data received from each route, and are closely tied to axum
 */

use tracing::debug;
// 1. Import the prelude - this contains everything needed for the server to function.
use webauthn_rs::prelude::*;

// 2. The first step a client (user) will carry out is requesting a credential to be
// registered. We need to provide a challenge for this. The work flow will be:
//
//          ┌───────────────┐     ┌───────────────┐      ┌───────────────┐
//          │ Authenticator │     │    Browser    │      │     Site      │
//          └───────────────┘     └───────────────┘      └───────────────┘
//                  │                     │                      │
//                  │                     │     1. Start Reg     │
//                  │                     │─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─▶│
//                  │                     │                      │
//                  │                     │     2. Challenge     │
//                  │                     │◀ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ┤
//                  │                     │                      │
//                  │  3. Select Token    │                      │
//             ─ ─ ─│◀ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─│                      │
//  4. Verify │     │                     │                      │
//                  │  4. Yield PubKey    │                      │
//            └ ─ ─▶│─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─▶                      │
//                  │                     │                      │
//                  │                     │  5. Send Reg Opts    │
//                  │                     │─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─▶│─ ─ ─
//                  │                     │                      │     │ 5. Verify
//                  │                     │                      │         PubKey
//                  │                     │                      │◀─ ─ ┘
//                  │                     │                      │─ ─ ─
//                  │                     │                      │     │ 6. Persist
//                  │                     │                      │       Credential
//                  │                     │                      │◀─ ─ ┘
//                  │                     │                      │
//                  │                     │                      │
//
// In this step, we are responding to the start reg(istration) request, and providing
// the challenge to the browser.

pub async fn start_register(
    Extension(app_state): Extension<AppState>,
    mut session: WritableSession,
    Path(username): Path<String>,
) -> Result<impl IntoResponse, WebauthnError> {
    debug!("Start register");
    // We get the username from the URL, but you could get this via form submission or
    // some other process. In some parts of Webauthn, you could also use this as a "display name"
    // instead of a username. Generally you should consider that the user *can* and *will* change
    // their username at any time.

    // Since a user's username could change at anytime, we need to bind to a unique id.
    // We use uuid's for this purpose, and you should generate these randomly. If the
    // username does exist and is found, we can match back to our unique id. This is
    // important in authentication, where presented credentials may *only* provide
    // the unique id, and not the username!

    let user_unique_id = {
        let users_guard = app_state.users.lock().await;
        users_guard
            .name_to_id
            .get(&username)
            .copied()
            .unwrap_or_else(Uuid::new_v4)
    };

    // Remove any previous registrations that may have occured from the session.
    session.remove("reg_state");

    // If the user has any other credentials, we exclude these here so they can't be duplicate registered.
    // It also hints to the browser that only new credentials should be "blinked" for interaction.
    let exclude_credentials = {
        let users_guard = app_state.users.lock().await;
        users_guard
            .keys
            .get(&user_unique_id)
            .map(|keys| keys.iter().map(|sk| sk.cred_id().clone()).collect())
    };

    let res = match app_state.webauthn.start_passkey_registration(
        user_unique_id,
        &username,
        &username,
        exclude_credentials,
    ) {
        Ok((ccr, reg_state)) => {
            // Note that due to the session store in use being a server side memory store, this is
            // safe to store the reg_state into the session since it is not client controlled and
            // not open to replay attacks. If this was a cookie store, this would be UNSAFE.
            session
                .insert("reg_state", (username, user_unique_id, reg_state))
                .expect("Failed to insert");
            debug!("Registration Successful!");
            Json(ccr)
        }
        Err(e) => {
            debug!("challenge_register -> {:?}", e);
            return Err(WebauthnError::Unknown);
        }
    };
    Ok(res)
}

// 3. The browser has completed it's steps and the user has created a public key
// on their device. Now we have the registration options sent to us, and we need
// to verify these and persist them.
pub async fn finish_register(
    Extension(app_state): Extension<AppState>,
    mut session: WritableSession,
    Json(reg): Json<RegisterPublicKeyCredential>,
) -> Result<impl IntoResponse, WebauthnError> {
    let (username, user_unique_id, reg_state): (String, Uuid, PasskeyRegistration) = session
        .get("reg_state")
        .ok_or(WebauthnError::CorruptSession)?; //Corrupt Session

    session.remove("reg_state");

    let res = match app_state
        .webauthn
        .finish_passkey_registration(&reg, &reg_state)
    {
        Ok(sk) => {
            let mut users_guard = app_state.users.lock().await;
            let connection = app_state.connection.lock().await;
            users_guard.register(user_unique_id, &sk, username, &connection);
            StatusCode::OK
        }
        Err(e) => {
            debug!("challenge_register -> {:?}", e);
            StatusCode::BAD_REQUEST
        }
    };

    login(user_unique_id, app_state, session).await;
    Ok(res)
}

// 4. Now that our public key has been registered, we can authenticate a user and verify
// that they are the holder of that security token. The work flow is similar to registration.
//
//          ┌───────────────┐     ┌───────────────┐      ┌───────────────┐
//          │ Authenticator │     │    Browser    │      │     Site      │
//          └───────────────┘     └───────────────┘      └───────────────┘
//                  │                     │                      │
//                  │                     │     1. Start Auth    │
//                  │                     │─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─▶│
//                  │                     │                      │
//                  │                     │     2. Challenge     │
//                  │                     │◀ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ┤
//                  │                     │                      │
//                  │  3. Select Token    │                      │
//             ─ ─ ─│◀ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─│                      │
//  4. Verify │     │                     │                      │
//                  │    4. Yield Sig     │                      │
//            └ ─ ─▶│─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─▶                      │
//                  │                     │    5. Send Auth      │
//                  │                     │        Opts          │
//                  │                     │─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─▶│─ ─ ─
//                  │                     │                      │     │ 5. Verify
//                  │                     │                      │          Sig
//                  │                     │                      │◀─ ─ ┘
//                  │                     │                      │
//                  │                     │                      │
//
// The user indicates the wish to start authentication and we need to provide a challenge.

pub async fn start_authentication(
    Extension(app_state): Extension<AppState>,
    mut session: WritableSession,
    Path(username): Path<String>,
) -> Result<impl IntoResponse, WebauthnError> {
    debug!("Start Authentication");
    // We get the username from the URL, but you could get this via form submission or
    // some other process.

    // Remove any previous authentication that may have occured from the session.
    session.remove("auth_state");

    // Get the set of keys that the user possesses
    let users_guard = app_state.users.lock().await;

    let connection = app_state.connection.lock().await;
    // Look up their unique id from the username
    let user = users_guard
        .by_username(&username, &connection)
        .ok_or(WebauthnError::UserNotFound)?;

    /*
    let allow_credentials = users_guard
        .keys
        .get(&user.id)
        .ok_or(WebauthnError::UserHasNoCredentials)?;
        */
    let allow_credentials = users_guard.passkey_credentials_for(&user.id, &connection);
    drop(connection);

    if allow_credentials.is_empty() {
        return Err(WebauthnError::UserHasNoCredentials);
    }

    let allow_credentials: Vec<Passkey> = allow_credentials
        .into_iter()
        .map(|ppk| ppk.passkey)
        .collect();

    let res = match app_state
        .webauthn
        .start_passkey_authentication(&allow_credentials)
    {
        Ok((rcr, auth_state)) => {
            // Drop the mutex to allow the mut borrows below to proceed
            drop(users_guard);

            // Note that due to the session store in use being a server side memory store, this is
            // safe to store the auth_state into the session since it is not client controlled and
            // not open to replay attacks. If this was a cookie store, this would be UNSAFE.
            session
                .insert("auth_state", (user.id, auth_state))
                .expect("Failed to insert");
            Json(rcr)
        }
        Err(e) => {
            debug!("challenge_authenticate -> {:?}", e);
            return Err(WebauthnError::Unknown);
        }
    };

    Ok(res)
}

// 5. The browser and user have completed their part of the processing. Only in the
// case that the webauthn authenticate call returns Ok, is authentication considered
// a success. If the browser does not complete this call, or *any* error occurs,
// this is an authentication failure.

pub async fn finish_authentication(
    Extension(app_state): Extension<AppState>,
    mut session: WritableSession,
    Json(auth): Json<PublicKeyCredential>,
) -> Result<impl IntoResponse, WebauthnError> {
    let (user_unique_id, auth_state): (Uuid, PasskeyAuthentication) = session
        .get("auth_state")
        .ok_or(WebauthnError::CorruptSession)?;

    session.remove("auth_state");

    let res = match app_state
        .webauthn
        .finish_passkey_authentication(&auth, &auth_state)
    {
        Ok(auth_result) => {
            let users_guard = app_state.users.lock().await;

            let connection = app_state.connection.lock().await;
            let mut allow_credentials =
                users_guard.passkey_credentials_for(&user_unique_id, &connection);

            if allow_credentials.is_empty() {
                return Err(WebauthnError::UserHasNoCredentials);
            }

            users_guard.update_credentials(&mut allow_credentials, &auth_result, &connection);

            StatusCode::OK
        }
        Err(e) => {
            debug!("challenge_register -> {:?}", e);
            StatusCode::BAD_REQUEST
        }
    };
    login(user_unique_id, app_state, session).await;
    debug!("Authentication Successful!");
    Ok(res)
}

#[derive(Elm, ElmEncode, Deserialize, Debug)]
pub struct LoginCredentials {
    pub username: String,
    pub password: String,
}

#[derive(Elm, ElmDecode, Serialize, Debug)]
pub enum LoginCredentialsResponse {
    SuccessfullyLoggedInWithCreds,
    LoginWithCredsNotFound,
}

pub async fn login_with_credentials(
    Extension(app_state): Extension<AppState>,
    session: WritableSession,
    username: String,
    password: String,
) -> LoginCredentialsResponse {
    let users = app_state.users.lock().await;
    let user =
        users.by_username_and_password(&username, &password, &app_state.connection.lock().await);
    drop(users);
    match user {
        Some(user) => {
            login(user.id, app_state, session).await;

            LoginCredentialsResponse::SuccessfullyLoggedInWithCreds
        }
        None => LoginCredentialsResponse::LoginWithCredsNotFound,
    }
}

pub const USER_INFO: &str = "USER_INFO";

async fn login(user_unique_id: Uuid, app_state: AppState, mut session: WritableSession) {
    session
        .insert("logged_in_user", user_unique_id)
        .expect("Unable to write to session");
    let session_id = SessionId::new();
    let user_info = (session_id.clone(), user_unique_id);
    session
        .insert(USER_INFO, user_info)
        .expect("Unable to write to session");
    let mut sessions_by_user = app_state.sessions_by_user.write().await;
    let sessions = sessions_by_user
        .entry(user_unique_id)
        .or_insert(HashSet::new());
    let lobby = RealmId::Lobby;
    app_state.grant_access(&user_unique_id, &lobby).await;
    sessions.insert(session_id);
}
