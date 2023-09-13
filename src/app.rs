use crate::{
    hades::{RealmId, RealmMsg, ToFrontend},
    startup::Cmd,
};

pub struct Model {
    counter: u32,
}

impl Model {
    pub fn new() -> Model {
        Model { counter: 0 }
    }

    pub fn update(&self, msg: RealmMsg, realm_id: RealmId) -> (Model, Cmd) {
        match msg {
            RealmMsg::Ping => {
                let model = Model {
                    counter: self.counter + 1,
                };

                let msgs = [ToFrontend::UpdateCounter(model.counter)];
                (model, Cmd::broadcast(realm_id, msgs))
            }
        }
    }
}
