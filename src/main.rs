use crate::{
    hades::{write_elm_types, ToFrontendEnvelope},
    startup::AppState,
};

use auth::{login, login_with_credentials, LoginCredentials, LoginCredentialsResponse, USER_INFO};
use axum::{
    extract::Extension,
    http::StatusCode,
    response::sse::{Event, Sse},
    response::IntoResponse,
    routing::{get, post},
    Json, Router,
};

use prometheus::{
    HistogramOpts, HistogramVec, IntCounter, IntCounterVec, IntGauge, Opts, Registry,
};

use axum_server::tls_rustls::RustlsConfig;
use axum_sessions::extractors::{ReadableSession, WritableSession};
use axum_sessions::{async_session::MemoryStore, SameSite, SessionLayer};
use elm_rs::{Elm, ElmDecode, ElmEncode};
use error::WebauthnError;
use futures::stream::Stream;
use hades::ToBackendEnvelope;
use lazy_static::lazy_static;
use rand::thread_rng;
use rand::Rng;
use serde::{Deserialize, Serialize};
use std::{convert::Infallible, net::SocketAddr, path::PathBuf, time::Duration};
use tokio::sync::mpsc;
use tokio_stream::{wrappers::ReceiverStream, StreamExt as _};
use tower_http::services::{ServeDir, ServeFile};
use tracing::debug;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};
use users::{SessionId, UserId};

mod app;
mod auth;
mod error;
mod game;
mod hades;
mod startup;
mod users;

use crate::auth::{finish_authentication, finish_register, start_authentication, start_register};

lazy_static! {
    pub static ref REGISTRY: Registry = Registry::new();
    pub static ref INCOMING_REQUESTS: IntCounter =
        IntCounter::new("incoming_requests", "Incoming Requests").expect("metric can be created");
    pub static ref CONNECTED_CLIENTS: IntGauge =
        IntGauge::new("connected_clients", "Connected Clients").expect("metric can be created");
    pub static ref RESPONSE_CODE_COLLECTOR: IntCounterVec = IntCounterVec::new(
        Opts::new("response_code", "Response Codes"),
        &["env", "statuscode", "type"]
    )
    .expect("metric can be created");
    pub static ref RESPONSE_TIME_COLLECTOR: HistogramVec = HistogramVec::new(
        HistogramOpts::new("response_time", "Response Times"),
        &["env"]
    )
    .expect("metric can be created");
}

fn register_custom_metrics() {
    REGISTRY
        .register(Box::new(INCOMING_REQUESTS.clone()))
        .expect("collector can be registered");

    REGISTRY
        .register(Box::new(CONNECTED_CLIENTS.clone()))
        .expect("collector can be registered");

    REGISTRY
        .register(Box::new(RESPONSE_CODE_COLLECTOR.clone()))
        .expect("collector can be registered");

    REGISTRY
        .register(Box::new(RESPONSE_TIME_COLLECTOR.clone()))
        .expect("collector can be registered");
}

#[tokio::main]
async fn main() {
    register_custom_metrics();
    /*
    let config = RustlsConfig::from_pem_file(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("self_signed_certs")
            .join("elmcards.ca.pem"),
        //.join("cert.pem"),
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("self_signed_certs")
            .join("elmcards.ca-key.pem"),
        //join("key.pem"),
    )
    .await
    .unwrap();
    */

    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "elm_webauthn=debug,tower_http=debug,".into())
                .add_directive("hyper::proto::h1::io=error".parse().unwrap())
                .add_directive("h2::codec=error".parse().unwrap())
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
        .with_http_only(false) // to allow remember session via JS
        .with_secure(false); // TODO: change this to true when running on an HTTPS/production server instead of locally
    let serve_dir = ServeDir::new("www/assets").not_found_service(ServeFile::new("www/index.html"));

    // build our application with a route
    let app = Router::new()
        .nest_service("/", ServeFile::new("www/index.html"))
        .nest_service("/assets", serve_dir)
        .route("/register_with_credentials", post(register_with_creds))
        .route("/register_start/:username", post(start_register))
        .route("/register_finish", post(finish_register))
        .route("/login_start/:username", post(start_authentication))
        .route("/login_finish", post(finish_authentication))
        .route(
            "/login_with_credentials",
            post(handle_login_with_credentials),
        )
        .route("/remember", get(remember_handler))
        .route("/send", post(send))
        .route("/events", get(sse_handler))
        .route("/metrics", get(metrics_handler))
        .layer(Extension(app_state))
        .layer(session_layer);

    // run our app with hyper
    // `axum::Server` is a re-export of `hyper::Server`
    /*
    let addr = SocketAddr::from(([0, 0, 0, 0], 8080));
    debug!("listening on {addr}");
    axum_server::bind_rustls(addr, config)
        .serve(app.into_make_service())
        .await
        .unwrap();
        */
    axum::Server::bind(&"0.0.0.0:8080".parse().unwrap())
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
    let session_id_opt = session.get(USER_INFO);
    let src = if let Some((session_id, user_id)) = session_id_opt {
        app_state
            .register_new_session(&session_id, &user_id)
            .await
            .unwrap()
    } else {
        unauthorized_stream().await
    };
    CONNECTED_CLIENTS.inc();
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

pub async fn remember_handler(session: ReadableSession) -> Result<impl IntoResponse, Infallible> {
    if let Some((_, _)) = session.get::<(SessionId, UserId)>(USER_INFO) {
        Ok("yay")
    } else {
        Ok("nay")
    }
}

pub async fn send(
    Extension(app_state): Extension<AppState>,
    session: ReadableSession,
    Json(envelope): Json<ToBackendEnvelope>,
) -> Result<impl IntoResponse, WebauthnError> {
    INCOMING_REQUESTS.inc();
    if let Some((session_id, user_id)) = session.get(USER_INFO) {
        match envelope {
            ToBackendEnvelope::ForRealm(realm_id, to_backend) => {
                app_state
                    .send_from_frontend(realm_id, to_backend, user_id, session_id)
                    .await;
            }
            ToBackendEnvelope::EnterRealm(realm_id) => {
                app_state
                    .enter_realm(&session_id, &user_id, &realm_id)
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

#[derive(Elm, ElmEncode, Deserialize, Debug)]
pub struct RegisterCredentials {
    username: String,
    password: String,
}

#[derive(Elm, ElmDecode, Serialize, Debug)]
pub enum RegisterCredentialsResponse {
    SuccessfullyRegisteredWithCreds,
    RegisteredWithCredsError(String),
}

pub async fn register_with_creds(
    Extension(app_state): Extension<AppState>,
    session: WritableSession,
    Json(creds): Json<RegisterCredentials>,
) -> Json<RegisterCredentialsResponse> {
    let users = app_state.users.lock().await;
    let user = users
        .register_with_credentials(&creds.username, &creds.password)
        .await;
    drop(users);

    match user {
        Ok(user) => {
            login(user.id, app_state, session).await;
            Json(RegisterCredentialsResponse::SuccessfullyRegisteredWithCreds)
        }
        Err(e) => Json(RegisterCredentialsResponse::RegisteredWithCredsError(e)),
    }
}

pub async fn handle_login_with_credentials(
    app_state: Extension<AppState>,
    session: WritableSession,
    Json(creds): Json<LoginCredentials>,
) -> Json<LoginCredentialsResponse> {
    Json(login_with_credentials(app_state, session, creds.username, creds.password).await)
}
async fn metrics_handler() -> Result<impl IntoResponse, String> {
    use prometheus::Encoder;
    let encoder = prometheus::TextEncoder::new();

    let mut buffer = Vec::new();
    if let Err(e) = encoder.encode(&REGISTRY.gather(), &mut buffer) {
        eprintln!("could not encode custom metrics: {}", e);
    };
    let mut res = match String::from_utf8(buffer.clone()) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("custom metrics could not be from_utf8'd: {}", e);
            String::default()
        }
    };
    buffer.clear();

    let mut buffer = Vec::new();
    if let Err(e) = encoder.encode(&prometheus::gather(), &mut buffer) {
        eprintln!("could not encode prometheus metrics: {}", e);
    };
    let res_custom = match String::from_utf8(buffer.clone()) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("prometheus metrics could not be from_utf8'd: {}", e);
            String::default()
        }
    };
    buffer.clear();

    res.push_str(&res_custom);
    Ok(res)
}
