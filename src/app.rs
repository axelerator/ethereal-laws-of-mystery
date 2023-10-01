use std::{collections::HashMap, sync::Arc};

use elm_rs::{Elm, ElmDecode, ElmEncode};
use serde::{Deserialize, Serialize};
use tokio::sync::Mutex;
use tracing::error;

use crate::{
    game::{Game, GameInfo, ToGame, Transition},
    hades::RealmId,
    startup::{Cmd, Realm},
    users::{SessionId, UserId, Users},
};
#[derive(Debug, Clone)]
pub enum Msg {
    NewGameStarted(Realm, Vec<UserId>),
    PlayerJoined(UserId),
    UserAlreadyWaiting(UserId),
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
    EnteredGame(RealmId, GameInfo),
}

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub enum ToFrontendLobby {
    GameStart(RealmId),
    WaitingForMorePlayers,
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
    StartGame,
    WaitForGame(usize),
}

#[derive(Default)]
pub struct Lobby {
    waiting: HashMap<usize, Vec<UserId>>,
}

impl Lobby {
    pub fn user_already_waiting(&self, user_id: &UserId) -> bool {
        for (_, user_ids) in self.waiting.iter() {
            if user_ids.contains(user_id) {
                return true;
            }
        }
        false
    }
}

impl RealmModel {
    pub fn new(hint: Option<NewRealmHint>) -> RealmModel {
        match hint {
            None => RealmModel::Lobby(Lobby::default()),
            Some(NewRealmHint::Game(user_ids)) => RealmModel::Game(Game::new(user_ids)),
        }
    }

    pub fn joined(&self, user_id: UserId, _session_id: SessionId) -> Option<Msg> {
        match self {
            RealmModel::Lobby(lobby) => {
                if lobby.user_already_waiting(&user_id) {
                    Some(Msg::UserAlreadyWaiting(user_id))
                } else {
                    None
                }
            }
            RealmModel::Game(_) => Some(Msg::PlayerJoined(user_id)),
        }
    }

    pub async fn update(
        self,
        msg: Msg,
        realm: Realm,
        users: &Arc<Mutex<Users>>,
    ) -> (RealmModel, Cmd) {
        match (self, msg) {
            (RealmModel::Lobby(lobby), Msg::NewGameStarted(new_realm, user_ids)) => {
                let cmds = user_ids.into_iter().map(|user_id| {
                    let game_start = |realm_id| {
                        ToFrontend::ToLobbyFrontend(ToFrontendLobby::GameStart(realm_id))
                    };
                    new_realm.add_user(user_id, game_start)
                });
                (RealmModel::Lobby(lobby), new_realm.batch(cmds))
            }
            (RealmModel::Game(game), Msg::PlayerJoined(user_id)) => {
                let updated_game = game.resolve_player_names(users).await;
                let game_info = updated_game.game_info(user_id);
                (
                    RealmModel::Game(updated_game),
                    realm.to_user(
                        user_id,
                        [ToFrontend::EnteredGame(realm.id.clone(), game_info)],
                    ),
                )
            }
            (RealmModel::Game(_), Msg::NewGameStarted(_, _)) => todo!(),
            (RealmModel::Lobby(l), Msg::PlayerJoined(_)) => (RealmModel::Lobby(l), realm.nothing()),
            (RealmModel::Lobby(l), Msg::UserAlreadyWaiting(user_id)) => {
                let cmd = realm.to_user(
                    user_id,
                    vec![ToFrontend::ToLobbyFrontend(
                        ToFrontendLobby::WaitingForMorePlayers,
                    )],
                );
                (RealmModel::Lobby(l), cmd)
            }
            (RealmModel::Game(_), Msg::UserAlreadyWaiting(_)) => todo!(),
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
                let (updated_game, transitions) = game.update(to_game, &user_id);
                let mut cmds = transitions
                    .into_iter()
                    .map(|(user_id, transition)| {
                        realm.to_user(user_id, [ToFrontend::ToGameFrontend(transition)])
                    })
                    .collect::<Vec<Cmd>>();
                if updated_game.is_over() {
                    let game_ended = |realm_id: RealmId| {
                        ToFrontend::ToGameFrontend(Transition::GameEnded(realm_id))
                    };
                    cmds.push(realm.close(game_ended));
                }
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
    mut lobby: Lobby,
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
            // You can also send msgs to individual sessions (browser windows/tabs)
            // realm.to_session(_session_id, [ToFrontend::UpdateCounter(counter)]),
            // Or to all sessions of a particular user
            // realm.to_user(_user_id, [ToFrontend::UpdateCounter(counter)]),
            // Or to everyone in the realm
            // realm.broadcast([ToFrontend::...
            // Or create an entire new realm
            // realm.spawn(NewRealmHint::Game, Msg::GotNewRealm)
        ),
        ToLobby::WaitForGame(number_of_players) => {
            let ppl_waiting = lobby.waiting.entry(number_of_players).or_insert(vec![]);
            if number_of_players < 4 && !ppl_waiting.contains(&user_id) {
                ppl_waiting.push(user_id);
                if ppl_waiting.len() == number_of_players {
                    let players = ppl_waiting.clone();
                    ppl_waiting.clear();
                    let cmd = realm.spawn(NewRealmHint::Game(players.clone()), |realm_id| {
                        Msg::NewGameStarted(realm_id, players)
                    });
                    return (lobby, cmd);
                } else {
                    let cmd = realm.to_user(
                        user_id,
                        vec![ToFrontend::ToLobbyFrontend(
                            ToFrontendLobby::WaitingForMorePlayers,
                        )],
                    );
                    return (lobby, cmd);
                }
            }
            (lobby, realm.nothing())
        }
    }
}
