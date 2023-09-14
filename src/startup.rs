use rusqlite::Connection;
use serde_rusqlite::*;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use tokio::sync::{mpsc::Sender, Mutex, RwLock};
use webauthn_rs::prelude::*;

use crate::app::{Msg, NewRealmHint, RealmModel};
use crate::app::{ToBackend, ToFrontend};
use crate::hades::{RealmId, ToFrontendEnvelope};
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
#[derive(Clone, Debug)]
pub struct Cmd {
    internal: CmdInternal,
}

#[derive(Clone, Debug)]
pub enum CmdInternal {
    None,
    Cmds(Vec<Cmd>),
    SendToUser(UserId, Vec<ToFrontend>),
    SendToSession(SessionId, Vec<ToFrontend>),
    BroadcastToRealm(RealmId, Vec<ToFrontend>),
    Spawn(RealmId, NewRealmHint, Msg),
    AddUserToRealm(RealmId, UserId),
}

type SessionsByUser = Arc<RwLock<HashMap<UserId, HashSet<SessionId>>>>;
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
    pub sessions_by_user: SessionsByUser,
    pub events_inbox_by_session_id: InboxesBySession,
}

pub enum RealmThreadMsg {
    FromFrontend(ToBackend, RealmId, UserId, SessionId),
    BackendMsg(Msg, RealmId),
}

pub struct Realms {
    pub realms: HashMap<RealmId, Sender<RealmThreadMsg>>,
    id_seq: u32,
}

impl Realms {
    pub async fn send_from_frontend(
        &self,
        realm_id: RealmId,
        to_backend: ToBackend,
        user_id: UserId,
        session_id: SessionId,
    ) {
        if let Some(realm) = self.realms.get(&realm_id) {
            if realm
                .send(RealmThreadMsg::FromFrontend(
                    to_backend, realm_id, user_id, session_id,
                ))
                .await
                .is_err()
            {
                todo!("track disconnected")
            }
        } else {
            error!("Realm {:?} not found!", realm_id);
        }
    }
    pub async fn send_backend_msg(&self, realm_id: RealmId, msg: Msg) {
        if let Some(realm) = self.realms.get(&realm_id) {
            if realm
                .send(RealmThreadMsg::BackendMsg(msg, realm_id))
                .await
                .is_err()
            {
                todo!("track disconnected")
            }
        } else {
            error!("Realm {:?} not found!", realm_id);
        }
    }
}

async fn process_cmd(
    cmd: CmdInternal,
    realm_members: &RealmMembers,
    inboxes: &InboxesBySession,
    sessions_by_user: &SessionsByUser,
    realm_mngr_inbox: RealmManagerInbox,
) {
    match cmd {
        CmdInternal::None => {}
        CmdInternal::Cmds(cmds) => panic!("Must have been flattened to avoid recursive async fn"),
        CmdInternal::SendToUser(user_id, to_frontends) => {
            let sessions_by_user = sessions_by_user.read().await;
            let session_ids = sessions_by_user
                .get(&user_id)
                .map_or(Some(HashSet::new()), |s| Some(s.clone()))
                .unwrap();

            let inboxes = inboxes.read().await;
            for session_id in session_ids {
                if let Some(inbox) = inboxes.get(&session_id) {
                    for to_f in to_frontends.iter() {
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
        CmdInternal::SendToSession(session_id, to_frontends) => {
            let inboxes = inboxes.read().await;
            if let Some(inbox) = inboxes.get(&session_id) {
                for to_f in to_frontends {
                    inbox
                        .send(ToFrontendEnvelope::FromRealm(to_f.clone()))
                        .await
                        .expect("Sending to inbox failed");
                }
            } else {
                panic!("Inbox not found");
            }
        }
        CmdInternal::BroadcastToRealm(realm_id, to_frontends) => {
            let realm_members = realm_members.read().await;
            let recipients = realm_members.get(&realm_id);
            match recipients {
                None => {
                    error!("Realm {:?} doesn't exist", realm_id);
                    return;
                }
                Some(recipients) => {
                    let inboxes = inboxes.read().await;
                    for session_id in recipients {
                        if let Some(inbox) = inboxes.get(session_id) {
                            for to_f in to_frontends.iter() {
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
        CmdInternal::Spawn(from_realm_id, new_realm_hint, target_id) => {
            realm_mngr_inbox
                .send(RealmManagerMsg::CreateNewRealm(from_realm_id, new_realm_hint, target_id))
                .await
                .unwrap();
        }
        CmdInternal::AddUserToRealm(realm_id, user_id) => {
            let mut realm_members = realm_members.write().await;
            realm_members
                .entry(realm_id)
                .and_modify(|members| {
                    members.insert(user_id);
                })
                .or_insert_with(|| HashSet::from([user_id]));
        }
    }
}

fn flatten_cmd(cmd: CmdInternal, accu: &mut Vec<CmdInternal>) {
    match cmd {
        CmdInternal::Cmds(cmds) => {
            for c in cmds {
                let internal = c.internal;
                flatten_cmd(internal, accu);
            }
        }
        _ => {
            accu.push(cmd);
        }
    }
}

pub async fn new_realm(
    realm_members: RealmMembers,
    inboxes: InboxesBySession,
    sessions_by_user: SessionsByUser,
    realm_mngr_inbox: RealmManagerInbox,
    hint: Option<NewRealmHint>,
) -> Sender<RealmThreadMsg> {
    let (cmd_inbox, mut cmd_receiver) = mpsc::channel::<Cmd>(32);
    tokio::spawn(async move {
        while let Some(cmd) = cmd_receiver.recv().await {
            let cmd = cmd.internal;
            let mut cmds = vec![];
            flatten_cmd(cmd, &mut cmds);
            for flat_cmd in cmds {
                process_cmd(
                    flat_cmd,
                    &realm_members,
                    &inboxes,
                    &sessions_by_user,
                    realm_mngr_inbox.clone(),
                )
                .await;
            }
        }
    });

    let (inbox, mut receiver) = mpsc::channel(32);
    tokio::spawn(async move {
        let mut model = RealmModel::new(hint);

        while let Some(realm_thread_msg) = receiver.recv().await {
            match realm_thread_msg {
                RealmThreadMsg::FromFrontend(message, realm_id, user_id, session_id) => {
                    debug!("Updating model for: {:?} with {:?}", realm_id, message);
                    let realm = Realm { id: realm_id };
                    let (updated_model, cmd) =
                        model.update_from_frontend(message, realm, user_id, session_id);
                    model = updated_model;
                    if let Err(_) = cmd_inbox.send(cmd).await {
                        todo!("Client disconnected, stop sending");
                    }
                }
                RealmThreadMsg::BackendMsg(msg, realm_id) => {
                    let realm = Realm { id: realm_id };
                    let (updated_model, cmd) = model.update(msg, realm);
                    model = updated_model;
                    if let Err(_) = cmd_inbox.send(cmd).await {
                        todo!("Client disconnected, stop sending");
                    }
                }
            }
        }
    });
    inbox
}

#[derive(Debug, Clone)]
pub struct Realm {
    pub id: RealmId,
}

impl Realm {
    pub fn nothing(&self) -> Cmd {
        Cmd {
            internal: CmdInternal::None,
        }
    }

    pub fn batch<CS>(&self, cmds: CS) -> Cmd
    where
        CS: IntoIterator<Item = Cmd>,
    {
        Cmd {
            internal: CmdInternal::Cmds(cmds.into_iter().collect()),
        }
    }

    pub fn broadcast<'a, I>(&self, msgs: I) -> Cmd
    where
        I: IntoIterator<Item = ToFrontend>,
    {
        let msgs: Vec<ToFrontend> = msgs.into_iter().collect();
        Cmd {
            internal: CmdInternal::BroadcastToRealm(self.id.clone(), msgs),
        }
    }

    pub fn to_user<'a, I>(&self, user_id: UserId, msgs: I) -> Cmd
    where
        I: IntoIterator<Item = ToFrontend>,
    {
        let msgs: Vec<ToFrontend> = msgs.into_iter().collect();
        Cmd {
            internal: CmdInternal::SendToUser(user_id, msgs),
        }
    }

    pub fn to_session<'a, I>(&self, session_id: SessionId, msgs: I) -> Cmd
    where
        I: IntoIterator<Item = ToFrontend>,
    {
        let msgs: Vec<ToFrontend> = msgs.into_iter().collect();
        Cmd {
            internal: CmdInternal::SendToSession(session_id, msgs),
        }
    }

    pub fn spawn<F>(&self, hint: NewRealmHint, got_new_realm: F) -> Cmd
    where
        F: FnOnce(Realm) -> Msg,
    {
        let realm_id = RealmId::Realm(Uuid::new_v4().to_string());
        let new_realm = Realm { id: realm_id };
        let to_backend = got_new_realm(new_realm);
        Cmd {
            internal: CmdInternal::Spawn(self.id.clone(), hint, to_backend),
        }
    }
    pub fn add_user(&self, user_id: UserId) -> Cmd {
        Cmd {
            internal: CmdInternal::AddUserToRealm(self.id.clone(), user_id),
        }
    }
}

impl Realms {
    pub fn new() -> Realms {
        Realms {
            realms: HashMap::new(),
            id_seq: 0,
        }
    }

    async fn create_realm_(
        &mut self,
        id: &RealmId,
        realm_members: RealmMembers,
        inboxes: InboxesBySession,
        sessions_by_user: SessionsByUser,
        realm_mngr_inbox: RealmManagerInbox,
        hint: Option<NewRealmHint>,
    ) {
        let inbox = new_realm(
            realm_members,
            inboxes,
            sessions_by_user,
            realm_mngr_inbox,
            hint,
        )
        .await;
        self.realms.insert(id.clone(), inbox);
    }

    pub async fn create_realm(
        &mut self,
        realm_members: RealmMembers,
        inboxes: InboxesBySession,
        sessions_by_user: SessionsByUser,
        realm_mngr_inbox: RealmManagerInbox,
        hint: Option<NewRealmHint>,
    ) -> RealmId {
        let realm_id = RealmId::Realm(Uuid::new_v4().to_string());
        self.create_realm_(
            &realm_id,
            realm_members,
            inboxes,
            sessions_by_user,
            realm_mngr_inbox,
            hint,
        )
        .await;
        realm_id
    }

    pub async fn create_lobby(
        &mut self,
        realm_members: RealmMembers,
        inboxes: InboxesBySession,
        sessions_by_user: SessionsByUser,
        realm_mngr_inbox: RealmManagerInbox,
    ) {
        let id = RealmId::Lobby;
        self.create_realm_(
            &id,
            realm_members,
            inboxes,
            sessions_by_user,
            realm_mngr_inbox,
            None,
        )
        .await;
    }
}

pub enum RealmManagerMsg {
    CreateNewRealm(RealmId, NewRealmHint, Msg),
}
type RealmManagerInbox = Sender<RealmManagerMsg>;

impl AppState {
    pub async fn is_member_of(&self, session_id: &SessionId, realm_id: &RealmId) -> bool {
        let realm_members = self.realm_members.read().await;
        let contains = realm_members
            .get(realm_id)
            .map(|members| members.contains(session_id));
        contains.unwrap_or(false)
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

        let (realm_mngr_inbox, mut realm_mngr_receiver) =
            mpsc::channel::<RealmManagerMsg>(32);

        let realms_ = realms.clone();
        let realm_members_ = realm_members.clone();
        let events_inbox_by_session_id_ = events_inbox_by_session_id.clone();
        let sessions_by_user_ = sessions_by_user.clone();
        let realm_mngr_inbox_ = realm_mngr_inbox.clone();
        tokio::spawn(async move {
            while let Some(msg) = realm_mngr_receiver.recv().await {
                match msg {
                    RealmManagerMsg::CreateNewRealm(src_realm_id, hint, msg) => {
                        let realm_id = realms_
                            .write()
                            .await
                            .create_realm(
                                realm_members_.clone(),
                                events_inbox_by_session_id_.clone(),
                                sessions_by_user_.clone(),
                                realm_mngr_inbox_.clone(),
                                Some(hint),
                            )
                            .await;
                        realm_members_
                            .write()
                            .await
                            .insert(realm_id, HashSet::new());
                        realms_
                            .read()
                            .await
                            .send_backend_msg(src_realm_id, msg)
                            .await;
                    }
                }
            }
        });

        realms
            .write()
            .await
            .create_lobby(
                realm_members.clone(),
                events_inbox_by_session_id.clone(),
                sessions_by_user.clone(),
                realm_mngr_inbox,
            )
            .await;
        realm_members
            .write()
            .await
            .insert(RealmId::Lobby, HashSet::new());

        AppState {
            webauthn,
            users,
            connection,
            realms,
            sessions_by_user,
            realm_members,
            events_inbox_by_session_id,
        }
    }
}
