use elm_rs::{Elm, ElmDecode, ElmEncode};
use serde::{Deserialize, Serialize};
use tracing::debug;

use crate::{
    hades::RealmId,
    startup::{Cmd, CmdInternal, Realm},
    users::{SessionId, UserId},
};
#[derive(Debug, Clone)]
pub enum Msg {
    GotNewRealm(RealmId),
}

#[derive(Elm, ElmEncode, Deserialize, Debug, Clone)]
pub enum ToBackend {
    Increment,
    Decrement,
}

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub enum ToFrontend {
    UpdateCounter(i32),
    NewRealm(RealmId),
}

pub struct Model {
    counter: i32,
}

impl Model {
    pub fn new() -> Model {
        Model { counter: 0 }
    }

    pub fn update(&self, msg: Msg, realm: Realm) -> (Model, Cmd) {
        match msg {
            Msg::GotNewRealm(_) => {
                debug!("New realm: {:?}", realm.id);
                (Model { ..*self }, realm.nothing())
            }
        }
    }

    pub fn update_from_frontend(
        &self,
        msg: ToBackend,
        realm: Realm,
        _user_id: UserId,
        _session_id: SessionId,
    ) -> (Model, Cmd) {
        match msg {
            ToBackend::Increment => {
                let counter = self.counter + 1;
                (
                    Model { counter, ..*self },
                    // broadcast sends the message to everyone **in the realm**
                    realm.broadcast([ToFrontend::UpdateCounter(counter)]),
                    // You can also send msgs to individual sessions (browser windows/tabs)
                    // realm.to_session(_session_id, [ToFrontend::UpdateCounter(counter)]),
                    // Or to all sessions of a particular user
                    // realm.to_user(_user_id, [ToFrontend::UpdateCounter(counter)]),
                    // Or create an entire new realm
                    // realm.spawn(Msg::GotNewRealm)
                )
            }
            ToBackend::Decrement => {
                let counter = self.counter - 1;
                (
                    Model { counter, ..*self },
                    realm.broadcast([ToFrontend::UpdateCounter(counter)]),
                )
            }
        }
    }
}
