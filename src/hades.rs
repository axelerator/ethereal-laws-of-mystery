use elm_rs::{Elm, ElmDecode, ElmEncode};
use serde::{Deserialize, Serialize};
use std::{fs::File, io::Write};

use crate::{
    app::{ToBackend, ToFrontend, ToFrontendLobby, ToLobby},
    game::{CardContent, GameInfo, GameState, Location, Operator, Opponent, ToGame, Transition},
    LoginCredentials, LoginCredentialsResponse, RegisterCredentials, RegisterCredentialsResponse,
};

// None = Lobby
#[derive(Elm, ElmDecode, ElmEncode, Deserialize, Serialize, PartialEq, Eq, Hash, Debug, Clone)]
pub enum RealmId {
    Lobby,
    Realm(String),
}

#[derive(Elm, ElmEncode, Deserialize, Debug)]
pub enum ToBackendEnvelope {
    ForRealm(RealmId, ToBackend),
    EnterRealm(RealmId),
}

#[derive(Elm, ElmDecode, Serialize, Clone, Debug)]
pub enum ToFrontendEnvelope {
    Noop,
    Unauthorized,
    FromRealm(ToFrontend),
}

pub fn write_elm_types() {
    println!("Writing elm types");

    let mut target = File::create("client/src/Hades.elm").unwrap();
    // elm_rs provides a macro for conveniently creating an Elm module with everything needed
    elm_rs::export!("Hades", &mut target, {
        // generates types and encoders for types implementing ElmEncoder
        encoders: [RealmId, ToBackendEnvelope, ToBackend, ToLobby, ToGame, Location, LoginCredentials, RegisterCredentials],
        // generates types and decoders for types implementing ElmDecoder
        decoders: [RealmId, ToFrontendEnvelope, ToFrontend, ToFrontendLobby, Transition, CardContent, Operator, GameInfo, Location, GameState, Opponent,LoginCredentialsResponse, RegisterCredentialsResponse ],
        // generates types and functions for forming queries for types implementing ElmQuery
        queries: [],
        // generates types and functions for forming queries for types implementing ElmQueryField
        query_fields: [],
    })
    .unwrap();

    target
        .write_all(format!("\nappVersion = {}\n", env!("VERSION")).as_bytes())
        .unwrap();
}
