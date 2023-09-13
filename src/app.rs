use elm_rs::{Elm, ElmDecode, ElmEncode};
use serde::{Deserialize, Serialize};

use crate::{hades::RealmId, startup::Cmd};

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub enum ToFrontend {
    UpdateCounter(u32),
}

#[derive(Elm, ElmEncode, Deserialize, Debug)]
pub enum ToBackend {
    Increment,
    Decrement,
}

pub struct Model {
    counter: u32,
}

impl Model {
    pub fn new() -> Model {
        Model { counter: 0 }
    }

    pub fn update(&self, msg: ToBackend, realm_id: RealmId) -> (Model, Cmd) {
        match msg {
            ToBackend::Increment => {
                let counter = self.counter + 1;
                (
                    Model { counter, ..*self },
                    Cmd::broadcast(realm_id, [ToFrontend::UpdateCounter(counter)]),
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
