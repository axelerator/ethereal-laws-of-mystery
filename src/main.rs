use crate::{
    hades::{write_elm_types, ToFrontendEnvelope},
    startup::AppState,
};
use axum::{
    extract::Extension,
    http::StatusCode,
    response::sse::{Event, Sse},
    response::IntoResponse,
    routing::{get, post},
    Json, Router,
};
use axum_sessions::extractors::ReadableSession;
use axum_sessions::{async_session::MemoryStore, SameSite, SessionLayer};
use error::WebauthnError;
use futures::stream::Stream;
use hades::{RealmId, ToBackendEnvelope};
use rand::thread_rng;
use rand::Rng;
use std::{convert::Infallible, net::SocketAddr, time::Duration};
use tokio::sync::mpsc;
use tokio_stream::{wrappers::ReceiverStream, StreamExt as _};
use tower_http::services::{ServeDir, ServeFile};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

mod app;
mod auth;
mod error;
mod game;
mod hades;
mod startup;
mod users;

use crate::auth::{finish_authentication, finish_register, start_authentication, start_register};

#[tokio::main]
async fn main() {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "elm_webauthn=debug,tower_http=debug,".into())
                .add_directive("hyper::proto::h1::io=error".parse().unwrap())
                .add_directive("hyper::proto::h1::conn=error".parse().unwrap()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

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
    tracing::debug!("listening on {addr}");
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();
}

async fn unauthorized_stream() -> ReceiverStream<ToFrontendEnvelope> {
    let (inbox, receiver) = mpsc::channel(1);
    tokio::spawn(async move {
        let _ = inbox
            .send_timeout(ToFrontendEnvelope::Unauthorized, Duration::from_secs(1))
            .await;
    });
    ReceiverStream::new(receiver)
}

async fn sse_handler(
    Extension(app_state): Extension<AppState>,
    session: ReadableSession,
) -> Sse<impl Stream<Item = Result<Event, Infallible>>> {
    let session_id_opt = session.get("user_info");
    let src = if let Some((session_id, user_id)) = session_id_opt {
        let lobby_id = RealmId::Lobby;
        if let Ok(s) = app_state
            .try_enter_realm(&session_id, &user_id, &lobby_id)
            .await
        {
            s
        } else {
            unauthorized_stream().await
        }
    } else {
        unauthorized_stream().await
    };
    let stream = src
        .map(|envelope| {
            Event::default()
                .json_data(envelope)
                .expect("Envelopes should always be serialiable")
        })
        .map(Ok);

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
    if let Some((session_id, user_id)) = session.get("id") {
        match envelope {
            ToBackendEnvelope::ForRealm(realm_id, to_backend) => {
                app_state
                    .send_from_frontend(realm_id, to_backend, user_id, session_id)
                    .await;
            }
            ToBackendEnvelope::EnterRealm(realm_id) => {
                app_state
                    .try_enter_realm(&session_id, &user_id, &realm_id)
                    .await
                    .unwrap();
            }
        }
        Ok(StatusCode::OK)
    } else {
        tracing::warn!("Received message without session");
        Ok(StatusCode::BAD_REQUEST)
    }
}
