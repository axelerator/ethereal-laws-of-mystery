use elm_rs::{Elm, ElmDecode, ElmEncode};
use serde::{Deserialize, Serialize};
use tracing::{debug, error};

use crate::{
    game::{Game, ToGame, Transition},
    hades::RealmId,
    startup::{Cmd, CmdInternal, Realm},
    users::{SessionId, UserId},
};
#[derive(Debug, Clone)]
pub enum Msg {
    NewGameStarted(Realm, Vec<UserId>),
    PlayerJoined(UserId),
}

#[derive(Elm, ElmEncode, Deserialize, Debug, Clone)]
pub enum ToBackend {
    ForLobby(ToLobby),
    ForGame(ToGame),
}

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub enum ToFrontend {
    ToLobbyFrontend(ToFrontendLobby),
    ToGameFrontend(Transition),
    EnteredGame(RealmId),
}

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub enum ToFrontendLobby {
    UpdateCounter(i32),
    GameStart(RealmId),
}

#[derive(Debug, Clone)]
pub enum NewRealmHint {
    Game(Vec<UserId>),
}

pub enum RealmModel {
    Lobby(Lobby),
    Game(Game),
}

#[derive(Elm, ElmEncode, Deserialize, Debug, Clone)]
pub enum ToLobby {
    Increment,
    Decrement,
    StartGame,
}

pub struct Lobby {
    counter: i32,
}

impl RealmModel {
    pub fn new(hint: Option<NewRealmHint>) -> RealmModel {
        match hint {
            None => RealmModel::Lobby(Lobby { counter: 0 }),
            Some(NewRealmHint::Game(user_ids)) => RealmModel::Game(Game::new(user_ids)),
        }
    }

    pub fn joined(&self, user_id: UserId, _session_id: SessionId) -> Option<Msg> {
        Some(Msg::PlayerJoined(user_id))
    }

    pub fn update(self, msg: Msg, realm: Realm) -> (RealmModel, Cmd) {
        match (self, msg) {
            (RealmModel::Lobby(lobby), Msg::NewGameStarted(new_realm, user_ids)) => {
                let game_start =
                    ToFrontend::ToLobbyFrontend(ToFrontendLobby::GameStart(new_realm.id.clone()));
                let cmds = user_ids
                    .into_iter()
                    .map(|user_id| new_realm.to_user(user_id, [game_start.clone()]))
                    .collect::<Vec<Cmd>>();
                (RealmModel::Lobby(lobby), new_realm.batch(cmds))
            }
            (RealmModel::Game(game), Msg::PlayerJoined(user_id)) => (
                RealmModel::Game(game),
                realm.to_user(user_id, [ToFrontend::EnteredGame(realm.id.clone())]),
            ),
            (RealmModel::Game(_), Msg::NewGameStarted(_, _)) => todo!(),
            (RealmModel::Lobby(_), Msg::PlayerJoined(_)) => todo!(),
        }
    }

    pub fn update_from_frontend(
        self,
        msg: ToBackend,
        realm: Realm,
        user_id: UserId,
        session_id: SessionId,
    ) -> (RealmModel, Cmd) {
        match (self, msg) {
            (RealmModel::Lobby(lobby), ToBackend::ForLobby(to_lobby)) => {
                let (lobby, cmd) =
                    update_lobby_from_frontend(lobby, to_lobby, realm, user_id, session_id);
                (RealmModel::Lobby(lobby), cmd)
            }

            (RealmModel::Game(game), ToBackend::ForGame(to_game)) => {
                let (updated_game, transitions) = game.update(to_game);
                let cmds = transitions
                    .into_iter()
                    .map(|(user_id, transition)| {
                        realm.to_user(user_id, [ToFrontend::ToGameFrontend(transition)])
                    })
                    .collect::<Vec<Cmd>>();
                (RealmModel::Game(updated_game), realm.batch(cmds))
            }
            (RealmModel::Lobby(lobby), ToBackend::ForGame(to_game)) => {
                error!("In lobby but got msg for game {:?}", to_game);
                (RealmModel::Lobby(lobby), realm.nothing())
            }
            (RealmModel::Game(game), ToBackend::ForLobby(to_game)) => {
                error!("In game but got msg for lobby {:?}", to_game);
                (RealmModel::Game(game), realm.nothing())
            }
        }
    }
}

fn update_lobby_from_frontend(
    lobby: Lobby,
    msg: ToLobby,
    realm: Realm,
    user_id: UserId,
    _session_id: SessionId,
) -> (Lobby, Cmd) {
    match msg {
        ToLobby::StartGame => (
            lobby,
            realm.spawn(NewRealmHint::Game(vec![user_id]), |realm_id| {
                Msg::NewGameStarted(realm_id, vec![user_id])
            }),
        ),
        ToLobby::Increment => {
            let counter = lobby.counter + 1;
            (
                Lobby { counter },
                // broadcast sends the message to everyone **in the realm**
                realm.broadcast([ToFrontend::ToLobbyFrontend(ToFrontendLobby::UpdateCounter(
                    counter,
                ))]),
                // You can also send msgs to individual sessions (browser windows/tabs)
                // realm.to_session(_session_id, [ToFrontend::UpdateCounter(counter)]),
                // Or to all sessions of a particular user
                // realm.to_user(_user_id, [ToFrontend::UpdateCounter(counter)]),
                // Or create an entire new realm
                // realm.spawn(NewRealmHint::Game, Msg::GotNewRealm)
            )
        }
        ToLobby::Decrement => {
            let counter = lobby.counter - 1;
            (
                Lobby { counter },
                realm.broadcast([ToFrontend::ToLobbyFrontend(ToFrontendLobby::UpdateCounter(
                    counter,
                ))]),
            )
        }
    }
}
