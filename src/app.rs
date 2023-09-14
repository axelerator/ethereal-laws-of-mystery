use elm_rs::{Elm, ElmDecode, ElmEncode};
use serde::{Deserialize, Serialize};

use crate::{
    hades::RealmId,
    startup::Cmd,
    users::{SessionId, UserId},
};

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub enum ToFrontend {
    UpdateCounter(i32),
}

#[derive(Elm, ElmEncode, Deserialize, Debug)]
pub enum ToBackend {
    Increment,
    Decrement,
}

pub struct Model {
    counter: i32,
}

impl Model {
    pub fn new() -> Model {
        Model { counter: 0 }
    }

    pub fn update(
        &self,
        msg: ToBackend,
        realm_id: RealmId,
        _user_id: UserId,
        _session_id: SessionId,
    ) -> (Model, Cmd) {
        match msg {
            ToBackend::Increment => {
                let counter = self.counter + 1;
                (
                    Model { counter, ..*self },
                    // broadcast sends the message to everyone **in the realm**
                    Cmd::broadcast(realm_id, [ToFrontend::UpdateCounter(counter)]),
                    // You can also send msgs to individual sessions (browser windows/tabs)
                    //Cmd::to_session(_session_id, [ToFrontend::UpdateCounter(counter)]),
                    // Or to all sessions of a particular user
                    //Cmd::to_user(_user_id, [ToFrontend::UpdateCounter(counter)]),
                )
            }
            ToBackend::Decrement => {
                let counter = self.counter - 1;
                (
                    Model { counter, ..*self },
                    Cmd::broadcast(realm_id, [ToFrontend::UpdateCounter(counter)]),
                )
            }
        }
    }
}
