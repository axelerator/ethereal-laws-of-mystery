use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{Mutex, RwLock, mpsc::{Receiver, Sender}};
use webauthn_rs::prelude::*;
use rusqlite::{Connection, Result};
use serde_rusqlite::*;

use crate::users::{Users, User};
use tokio::sync::mpsc;

/*
 * Webauthn RS server side app state and setup  code.
 */

// Configure the Webauthn instance by using the WebauthnBuilder. This defines
// the options needed for your site, and has some implications. One of these is that
// you can NOT change your rp_id (relying party id), without invalidating all
// webauthn credentials. Remember, rp_id is derived from your URL origin, meaning
// that it is your effective domain name.

type RealmId = u32;
pub struct Realm {
    id: RealmId,
}

struct Model {
    counter: u32
}

#[derive(Clone, Debug)]
enum ToFrontend {
    UpdateCounter(u32)
}

#[derive(Clone, Debug)]
enum Cmd {
    None,
    Cmds(Vec<Cmd>),
    SendToUser(ToFrontend),
    SendToSession(ToFrontend),
    BroadcastToRealm(Vec<ToFrontend>),
}

impl Cmd {
    pub fn none() -> Cmd {
        Cmd::None
    }

    pub fn batch<CS>(cmds: CS) -> Cmd 
    where
        CS: for<'a> Into<&'a [Cmd]>
    {
        Cmd::Cmds(cmds.into().to_vec())
    }

    pub fn broadcast<'a, I>(msgs: I) -> Cmd
    where
        I: IntoIterator<Item = ToFrontend>,
    {
        let msgs : Vec<ToFrontend> = msgs.into_iter().collect();
        Cmd::BroadcastToRealm(msgs)
    }

}

impl Model {
    pub fn update(&self) -> (Model, Cmd) {
        let model = Model {
            counter: self.counter + 1
        };

        let msgs = [ToFrontend::UpdateCounter(model.counter)];
        (model, Cmd::broadcast(msgs))
    }
}

type RealmMsg = i32;

impl Realm {
}

#[derive(Clone)]
pub struct AppState {
    // Webauthn has no mutable inner state, so Arc and read only is sufficent.
    // Alternately, you could use a reference here provided you can work out
    // lifetimes.
    pub webauthn: Arc<Webauthn>,
    // This needs mutability, so does require a mutex.
    pub users: Arc<Mutex<Users>>,
    pub connection: Arc<Mutex<Connection>>,
    pub realms: Arc<RwLock<Realms>>,
}


pub struct Realms {
    realms: HashMap<RealmId, Sender<RealmMsg>>, 
    id_seq: RealmId,
}


pub async fn new_realm() -> Sender<RealmMsg> {
    let (inbox, mut receiver) = mpsc::channel(32);

    tokio::spawn(async move {
        let mut model = Model { counter: 0 };

        while let Some(_message) = receiver.recv().await {
            let (updated_model, cmd) = model.update();
            model = updated_model;
        }
    });
    inbox
}

impl Realms {
    pub fn new() -> Realms {
        Realms {
            realms: HashMap::new(),
            id_seq: 0
        }
    }

    pub async fn create_realm(&mut self) -> RealmId {
        let inbox = new_realm().await;
        self.realms.insert(self.id_seq, inbox);
        self.id_seq = self.id_seq + 1;
        self.id_seq
    }
}


impl AppState {

    pub async fn test(&self) {
        let conn = self.connection.lock().await;
        let mut statement = conn.prepare("SELECT * FROM users").unwrap();
        let res = from_rows::<User>(statement.query([]).unwrap()); 

        for user in res {
            println!("User: {}", user.unwrap().name);
        }
    }

    pub fn new() -> Self {
        // Effective domain name.
        let rp_id = "localhost";
        // Url containing the effective domain name
        // MUST include the port number!
        let rp_origin = Url::parse("http://localhost:8080").expect("Invalid URL");
        let builder = WebauthnBuilder::new(rp_id, &rp_origin).expect("Invalid configuration");

        // Now, with the builder you can define other options.
        // Set a "nice" relying party name. Has no security properties and
        // may be changed in the future.
        let builder = builder.rp_name("Axum Webauthn-rs");

        // Consume the builder and create our webauthn instance.
        let webauthn = Arc::new(builder.build().expect("Invalid configuration"));

        let users = Arc::new(Mutex::new(Users {
            name_to_id: HashMap::new(),
            keys: HashMap::new(),
        }));
        let connection = 
            Arc::new(Mutex::new(Connection::open("db.sqlite").expect("Didn't find sqlite DB")));
        let realms = Arc::new(RwLock::new(Realms::new()));
        let realm_id_seq = Arc::new(Mutex::new(0));
        AppState { webauthn, users, connection, realms }
    }
}
