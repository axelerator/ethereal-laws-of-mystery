use serde::{Deserialize, Serialize};
use std::fs::File;

use elm_rs::{Elm, ElmDecode, ElmEncode};

pub type RealmId = u32;

#[derive(Elm, ElmEncode, Deserialize, Debug)]
pub enum RealmMsg {
    Ping,
}

#[derive(Elm, ElmEncode, Deserialize, Debug)]
pub enum ToBackendEnvelope {
    ForRealm(RealmId, RealmMsg),
}

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub enum ToFrontend {
    UpdateCounter(u32),
}

#[derive(Elm, ElmDecode, Serialize, Clone, Debug)]
pub enum ToFrontendEnvelope {
    Noop,
    Unauthorized,
    FromRealm(ToFrontend),
}

pub fn write_elm_types() {
    let mut target = File::create("client/src/Hades.elm").unwrap();
    // elm_rs provides a macro for conveniently creating an Elm module with everything needed
    elm_rs::export!("Hades", &mut target, {
        // generates types and encoders for types implementing ElmEncoder
        encoders: [ToBackendEnvelope, RealmMsg],
        // generates types and decoders for types implementing ElmDecoder
        decoders: [ToFrontendEnvelope, ToFrontend],
        // generates types and functions for forming queries for types implementing ElmQuery
        queries: [],
        // generates types and functions for forming queries for types implementing ElmQueryField
        query_fields: [],
    })
    .unwrap();
}
