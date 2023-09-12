use axum::{extract::Extension, routing::{post, get}, Router};
use webauthn_rs::prelude::{PasskeyAuthentication, Uuid};
use std::net::SocketAddr;
use rand::Rng;
use rand::thread_rng;
use crate::{startup::AppState, users::User};
use axum_sessions::{async_session::MemoryStore, SameSite, SessionLayer};
use tower_http::services::{ServeDir, ServeFile};
use axum_sessions::extractors::{WritableSession, ReadableSession};


mod auth;
mod error;
mod startup;
mod users;

use crate::auth::{finish_authentication, finish_register, start_authentication, start_register};

#[tokio::main]
async fn main() {
    // Create the app
    let app_state = AppState::new();

    app_state.test().await;

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
        .route("/test", get(test_handler))
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


pub async fn test_handler(
    Extension(app_state): Extension<AppState>,
     session: ReadableSession,
) -> String {
    let auth_state : Option<(Uuid, PasskeyAuthentication)> = session.get("auth_state");
    match auth_state {
        Some((user_unique_id, auth_state)) => {
            println!("user_unique_id: {} auth_state: {:?}", user_unique_id, auth_state );
        },
        None => {
            println!("No auth state");
        }
    };
    println!("logged_in_user: {:?}", session.get::<Uuid>("logged_in_user"));

    "yeah".to_string()
}
