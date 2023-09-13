use rusqlite::Connection;
use serde_rusqlite::*;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use tokio::sync::{mpsc::Sender, Mutex, RwLock};
use webauthn_rs::prelude::*;

use crate::hades::{RealmId, RealmMsg, ToFrontend, ToFrontendEnvelope};
use crate::users::{SessionId, UserId};
use crate::{hades::ToBackendEnvelope, users::Users};
use tokio::sync::mpsc;
use tracing::{debug, error};

/*
 * Webauthn RS server side app state and setup  code.
 */

// Configure the Webauthn instance by using the WebauthnBuilder. This defines
// the options needed for your site, and has some implications. One of these is that
// you can NOT change your rp_id (relying party id), without invalidating all
// webauthn credentials. Remember, rp_id is derived from your URL origin, meaning
// that it is your effective domain name.

struct Model {
    counter: u32,
}

#[derive(Clone, Debug)]
enum Cmd {
    None,
    Cmds(Vec<Cmd>),
    SendToUser(ToFrontend),
    SendToSession(ToFrontend),
    BroadcastToRealm(RealmId, Vec<ToFrontend>),
}

impl Cmd {
    pub fn none() -> Cmd {
        Cmd::None
    }

    pub fn batch<CS>(cmds: CS) -> Cmd
    where
        CS: for<'a> Into<&'a [Cmd]>,
    {
        Cmd::Cmds(cmds.into().to_vec())
    }

    pub fn broadcast<'a, I>(realm_id: RealmId, msgs: I) -> Cmd
    where
        I: IntoIterator<Item = ToFrontend>,
    {
        let msgs: Vec<ToFrontend> = msgs.into_iter().collect();
        Cmd::BroadcastToRealm(realm_id, msgs)
    }
}

impl Model {
    pub fn update(&self) -> (Model, Cmd) {
        let model = Model {
            counter: self.counter + 1,
        };

        let msgs = [ToFrontend::UpdateCounter(model.counter)];
        (model, Cmd::broadcast(0, msgs))
    }
}

type RealmMembers = Arc<RwLock<HashMap<RealmId, HashSet<SessionId>>>>;
type InboxesBySession = Arc<RwLock<HashMap<SessionId, Sender<ToFrontendEnvelope>>>>;

#[derive(Clone)]
pub struct AppState {
    // Webauthn has no mutable inner state, so Arc and read only is sufficent.
    // Alternately, you could use a reference here provided you can work out
    // lifetimes.
    pub webauthn: Arc<Webauthn>,
    pub users: Arc<Mutex<Users>>,
    pub connection: Arc<Mutex<Connection>>,
    pub realms: Arc<RwLock<Realms>>,
    pub realm_members: RealmMembers,
    pub sessions_by_user: Arc<RwLock<HashMap<UserId, HashSet<SessionId>>>>,
    pub events_inbox_by_session_id: InboxesBySession,
    pub lobby: RealmId,
}

pub struct Realms {
    pub realms: HashMap<RealmId, Sender<RealmMsg>>,
    id_seq: RealmId,
}

impl Realms {
    pub async fn send(&self, envelope: ToBackendEnvelope) {
        match envelope {
            ToBackendEnvelope::ForRealm(realm_id, realm_msg) => {
                println!("REALM MSG: {:?}", realm_msg);

                if let Some(realm) = self.realms.get(&realm_id) {
                    Some(realm.send(realm_msg).await);
                } else {
                    println!("Realm not found!");
                }
            }
        }
    }
}

pub async fn new_realm(realm_members: RealmMembers, inboxes: InboxesBySession) -> Sender<RealmMsg> {
    let (cmd_inbox, mut cmd_receiver) = mpsc::channel(32);
    tokio::spawn(async move {
        while let Some(cmd) = cmd_receiver.recv().await {
            match cmd {
                Cmd::None => {}
                Cmd::Cmds(_) => todo!(),
                Cmd::SendToUser(_) => todo!(),
                Cmd::SendToSession(_) => todo!(),
                Cmd::BroadcastToRealm(realm_id, to_fs) => {
                    let realm_members = realm_members.read().await;
                    let recipients = realm_members.get(&realm_id);
                    match recipients {
                        None => {
                            error!("Realm {} doesn't exist", realm_id);
                            return;
                        }
                        Some(recipients) => {
                            let inboxes = inboxes.read().await;
                            for session_id in recipients {
                                if let Some(inbox) = inboxes.get(&session_id) {
                                    for to_f in to_fs.iter() {
                                        inbox
                                            .send(ToFrontendEnvelope::FromRealm(to_f.clone()))
                                            .await
                                            .expect("Sending to inbox failed");
                                    }
                                } else {
                                    panic!("Inbox not found");
                                }
                            }
                        }
                    }
                }
            }
        }
    });

    let (inbox, mut receiver) = mpsc::channel(32);
    tokio::spawn(async move {
        let mut model = Model { counter: 0 };

        while let Some(message) = receiver.recv().await {
            debug!("Updating model for: {:?}", message);
            let (updated_model, cmd) = model.update();
            model = updated_model;
            if let Err(_) = cmd_inbox.send(cmd).await {
                todo!("Client disconnected, stop sending");
            }
        }
    });
    inbox
}

impl Realms {
    pub fn new() -> Realms {
        Realms {
            realms: HashMap::new(),
            id_seq: 0,
        }
    }

    pub async fn create_realm(
        &mut self,
        realm_members: RealmMembers,
        inboxes: InboxesBySession,
    ) -> RealmId {
        let inbox = new_realm(realm_members, inboxes).await;
        let realm_id = self.id_seq;
        self.realms.insert(realm_id, inbox);
        self.id_seq = self.id_seq + 1;
        realm_id
    }
}

impl AppState {
    pub async fn is_member_of(&self, session_id: &SessionId, realm_id: RealmId) -> bool {
        let realm_members = self.realm_members.read().await;
        let contains = realm_members
            .get(&realm_id)
            .and_then(|members| Some(members.contains(session_id)));
        match contains {
            None => false,
            Some(c) => c,
        }
    }

    pub async fn new() -> Self {
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
        let connection = Arc::new(Mutex::new(
            Connection::open("db.sqlite").expect("Didn't find sqlite DB"),
        ));
        let realms = Arc::new(RwLock::new(Realms::new()));
        let realm_members = Arc::new(RwLock::new(HashMap::new()));
        let sessions_by_user = Arc::new(RwLock::new(HashMap::new()));
        let events_inbox_by_session_id = Arc::new(RwLock::new(HashMap::new()));

        let lobby = realms
            .write()
            .await
            .create_realm(realm_members.clone(), events_inbox_by_session_id.clone())
            .await;
        realm_members.write().await.insert(lobby, HashSet::new());

        AppState {
            webauthn,
            users,
            connection,
            realms,
            sessions_by_user,
            realm_members,
            lobby,
            events_inbox_by_session_id,
        }
    }
}
