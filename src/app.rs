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

#[derive(Debug, Clone)]
pub enum NewRealmHint {
    Game,
}

pub enum RealmModel {
    Lobby(Lobby),
    Game { counter: i32 },
}

pub struct Lobby {
    counter: i32,
}

impl RealmModel {
    pub fn new(hint: Option<NewRealmHint>) -> RealmModel {
        match hint {
            None => RealmModel::Lobby(Lobby { counter: 0 }),
            Some(NewRealmHint::Game) => RealmModel::Game { counter: 0 },
        }
    }

    pub fn update(self, msg: Msg, realm: Realm) -> (RealmModel, Cmd) {
        match msg {
            Msg::GotNewRealm(_) => {
                debug!("New realm: {:?}", realm.id);
                (self, realm.nothing())
            }
        }
    }

    pub fn update_from_frontend(
        self,
        msg: ToBackend,
        realm: Realm,
        user_id: UserId,
        session_id: SessionId,
    ) -> (RealmModel, Cmd) {
        match self {
            RealmModel::Lobby(lobby) => {
                let (lobby, cmd) =
                    update_lobby_from_frontend(lobby, msg, realm, user_id, session_id);
                (RealmModel::Lobby(lobby), cmd)
            }

            RealmModel::Game { counter } => {
                todo!()
            }
        }
    }
}

fn update_lobby_from_frontend(
    lobby: Lobby,
    msg: ToBackend,
    realm: Realm,
    _user_id: UserId,
    _session_id: SessionId,
) -> (Lobby, Cmd) {
    match msg {
        ToBackend::Increment => {
            let counter = lobby.counter + 1;
            (
                Lobby { counter },
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
            let counter = lobby.counter - 1;
            (
                Lobby { counter },
                realm.broadcast([ToFrontend::UpdateCounter(counter)]),
            )
        }
    }
}
