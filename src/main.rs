use crate::{
    hades::{write_elm_types, ToFrontendEnvelope},
    startup::AppState,
    users::SessionId,
};
use axum::{
    extract::Extension,
    headers,
    http::StatusCode,
    response::sse::{Event, Sse},
    response::IntoResponse,
    routing::{get, post},
    Json, Router, TypedHeader,
};
use axum_sessions::extractors::{ReadableSession, WritableSession};
use axum_sessions::{async_session::MemoryStore, SameSite, SessionLayer};
use error::WebauthnError;
use futures::stream::{self, Stream};
use hades::ToBackendEnvelope;
use rand::thread_rng;
use rand::Rng;
use std::{convert::Infallible, net::SocketAddr, time::Duration};
use tokio::sync::mpsc;
use tokio_stream::{wrappers::ReceiverStream, StreamExt as _};
use tower_http::services::{ServeDir, ServeFile};
use webauthn_rs::prelude::{PasskeyAuthentication, RegisterPublicKeyCredential, Uuid};
mod auth;
mod error;
mod hades;
mod startup;
mod users;

use crate::auth::{finish_authentication, finish_register, start_authentication, start_register};

#[tokio::main]
async fn main() {
    write_elm_types();
    // Create the app
    let app_state = AppState::new().await;

    //Configure cookie based sessions
    let store = MemoryStore::new();
    //let mut secret: [u8; 128] = [0; 128];
    //thread_rng().fill_bytes(secret);
    let secret = thread_rng().gen::<[u8; 128]>(); // MUST be at least 64 bytes!
                                                  //
    let session_layer = SessionLayer::new(store, &secret)
        .with_cookie_name("webauthnrs")
        .with_same_site_policy(SameSite::Lax)
        .with_secure(false); // TODO: change this to true when running on an HTTPS/production server instead of locally
    let serve_dir = ServeDir::new("www/assets").not_found_service(ServeFile::new("www/index.html"));

    // build our application with a route
    let app = Router::new()
        .nest_service("/", ServeFile::new("www/index.html"))
        .nest_service("/assets", serve_dir)
        .route("/register_start/:username", post(start_register))
        .route("/register_finish", post(finish_register))
        .route("/login_start/:username", post(start_authentication))
        .route("/login_finish", post(finish_authentication))
        .route("/send", post(send))
        .route("/events", get(sse_handler))
        .layer(Extension(app_state))
        .layer(session_layer);

    // run our app with hyper
    // `axum::Server` is a re-export of `hyper::Server`
    let addr = SocketAddr::from(([0, 0, 0, 0], 8080));
    println!("listening on {addr}");
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();
}

async fn sse_handler(
    Extension(app_state): Extension<AppState>,
    session: ReadableSession,
) -> Sse<impl Stream<Item = Result<Event, Infallible>>> {

    let session_id: SessionId = session.get("id").unwrap();
    let (inbox, receiver) = mpsc::channel(32);

    let mut event_inboxes = app_state.events_inbox_by_session_id.write().await;
    event_inboxes.insert(session_id, inbox);
    drop(event_inboxes);
    let stream = ReceiverStream::new(receiver)
        .map(|envelope| Event::default().json_data(envelope).unwrap())
        .map(Ok);
    let mut realm_members = app_state.realm_members.write().await;
    let members = realm_members.get_mut(&app_state.lobby).unwrap();
    members.insert(session_id);
    drop(realm_members);
    println!(
        "members.insert(session_id); {:?} isMO: {} - asl: {}",
        session_id,
        app_state.is_member_of(&session_id, 0).await,
        &app_state.lobby
    );

    Sse::new(stream).keep_alive(
        axum::response::sse::KeepAlive::new()
            .interval(Duration::from_secs(1))
            .text("keep-alive-text"),
    )
}
pub async fn send(
    Extension(app_state): Extension<AppState>,
    session: ReadableSession,
    Json(envelope): Json<ToBackendEnvelope>,
) -> Result<impl IntoResponse, WebauthnError> {
    if let Some(session_id) = session.get::<Uuid>("id") {
        match envelope {
            ToBackendEnvelope::ForRealm(realm_id, _) => {
                println!("Before");
                if app_state.is_member_of(&session_id, realm_id).await {
                    println!("a member of {} sends {:?}", realm_id, envelope);
                    app_state.realms.read().await.send(envelope).await;
                } else {
                    println!("Realms {:?}", app_state.realm_members.read().await.get(&0));
                    println!("{} not a member of {}", session_id, realm_id);
                }
            }
        }
        Ok(StatusCode::OK)
    } else {
        println!("No auth state");
        Ok(StatusCode::BAD_REQUEST)
    }
}
