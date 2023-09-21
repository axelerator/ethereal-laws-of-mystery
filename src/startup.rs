use rusqlite::Connection;
use serde_rusqlite::*;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use tokio::sync::{mpsc::Sender, Mutex, RwLock};
use tokio_stream::wrappers::ReceiverStream;
use webauthn_rs::prelude::*;

use crate::app::{Msg, NewRealmHint, RealmModel};
use crate::app::{ToBackend, ToFrontend};
use crate::hades::{RealmId, ToFrontendEnvelope};
use crate::users::Users;
use crate::users::{SessionId, UserId};
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
    Spawn(RealmId, RealmId, NewRealmHint, Msg),
    AddUserToRealm(RealmId, ToFrontend, UserId),
    Close(RealmId),
}

impl From<CmdInternal> for Cmd {
    fn from(internal: CmdInternal) -> Self {
        Cmd { internal }
    }
}

type SessionsByUser = Arc<RwLock<HashMap<UserId, HashSet<SessionId>>>>;
type RealmMembers = Arc<RwLock<RealmMembersStore>>;
type InboxesBySession = Arc<RwLock<HashMap<SessionId, Sender<ToFrontendEnvelope>>>>;

#[derive(Default)]
pub struct RealmMembersStore {
    // Define which users are allowed to join a certain realm
    users_by_realm: HashMap<RealmId, HashSet<UserId>>,

    // tracks which sessions are actually present in a realm
    sessions_by_realm: HashMap<RealmId, HashSet<SessionId>>,
}

impl RealmMembersStore {
    pub fn is_allowed_to_join(&self, user_id: &UserId, realm_id: &RealmId) -> bool {
        self.is_user_member_of(user_id, realm_id)
    }

    pub fn is_user_member_of(&self, user_id: &UserId, realm_id: &RealmId) -> bool {
        self.users_by_realm
            .get(realm_id)
            .map(|users| users.get(user_id))
            .is_some()
    }

    pub fn is_session_member_of(&self, session_id: &SessionId, realm_id: &RealmId) -> bool {
        self.sessions_by_realm
            .get(realm_id)
            .map(|sessions| sessions.get(session_id))
            .is_some()
    }

    pub fn grant_access(&mut self, user_id: &UserId, realm_id: &RealmId) {
        self.users_by_realm
            .entry(realm_id.clone())
            .and_modify(|members| {
                members.insert(*user_id);
            })
            .or_insert_with(|| HashSet::from([*user_id]));
    }

    pub fn enter_realm(&mut self, user_id: &UserId, session_id: &SessionId, realm_id: &RealmId) {
        debug!(
            "User({user_id}) with Session({:?}) entering: {:?}",
            session_id, realm_id
        );

        if self.is_allowed_to_join(user_id, realm_id) {
            self.sessions_by_realm
                .entry(realm_id.clone())
                .and_modify(|members| {
                    members.insert(session_id.clone());
                })
                .or_insert_with(|| HashSet::from([session_id.clone()]));
        } else {
            error!(
                "User({:?}) with session({:?}) tried to enter {:?} but isn't a member!",
                user_id, session_id, realm_id
            );
        }
    }

    pub fn leave_all_realms(&mut self, session_id: &SessionId) {
        // todo: make more efficient by storing inverse relationship
        for realm in self.sessions_by_realm.iter_mut() {
            let (_, members) = realm;
            members.remove(session_id);
        }
    }

    fn sessions_in_realm(&self, realm_id: &RealmId) -> HashSet<SessionId> {
        self.sessions_by_realm
            .get(realm_id)
            .cloned()
            .unwrap_or_default()
    }

    pub fn memberships(&self, user_id: &UserId) -> Vec<RealmId> {
        // todo: do this more efficiently
        let mut realm_ids = vec![];
        for (realm_id, users) in self.users_by_realm.iter() {
            if users.contains(user_id) {
                realm_ids.push(realm_id.clone());
            }
        }
        realm_ids
    }
}

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
    SendJoin(RealmId, UserId, SessionId),
}

pub struct Realms {
    pub realms: HashMap<RealmId, Sender<RealmThreadMsg>>,
}

impl Realms {
    pub async fn send_from_frontend(
        &self,
        realm_id: RealmId,
        to_backend: ToBackend,
        user_id: UserId,
        session_id: SessionId,
    ) {
        self.send_msg(
            realm_id.clone(),
            RealmThreadMsg::FromFrontend(to_backend, realm_id, user_id, session_id),
        )
        .await;
    }

    pub async fn send_backend_msg(&self, realm_id: RealmId, msg: Msg) {
        self.send_msg(realm_id.clone(), RealmThreadMsg::BackendMsg(msg, realm_id))
            .await
    }

    pub async fn send_joined_msg(
        &self,
        realm_id: &RealmId,
        user_id: &UserId,
        session_id: &SessionId,
    ) {
        self.send_msg(
            realm_id.clone(),
            RealmThreadMsg::SendJoin(realm_id.clone(), *user_id, session_id.clone()),
        )
        .await
    }

    async fn send_msg(&self, realm_id: RealmId, msg: RealmThreadMsg) {
        if let Some(realm) = self.realms.get(&realm_id) {
            if realm.send(msg).await.is_err() {
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
    debug!("Processing Cmd: {:?}", cmd);
    let stale_sessions = match cmd {
        CmdInternal::None => {
            vec![]
        }
        CmdInternal::Cmds(_) => panic!("Must have been flattened to avoid recursive async fn"),
        CmdInternal::SendToUser(user_id, to_frontends) => {
            let sessions_by_user = sessions_by_user.read().await;
            let session_ids = sessions_by_user
                .get(&user_id)
                .map_or(Some(HashSet::new()), |s| Some(s.clone()))
                .unwrap();

            send_to_sessions(session_ids, to_frontends, inboxes).await
        }
        CmdInternal::SendToSession(session_id, to_frontends) => {
            let mut stale_sessions = vec![];
            let inboxes = inboxes.read().await;
            if let Some(inbox) = inboxes.get(&session_id) {
                for to_f in to_frontends {
                    if let Err(_) = inbox
                        .send(ToFrontendEnvelope::FromRealm(to_f.clone()))
                        .await
                    {
                        stale_sessions.push(session_id.clone());
                    }
                }
            } else {
                panic!("Inbox not found");
            }
            stale_sessions
        }
        CmdInternal::BroadcastToRealm(realm_id, to_frontends) => {
            let realm_members = realm_members.read().await;
            let recipients = realm_members.sessions_in_realm(&realm_id);
            send_to_sessions(recipients.clone(), to_frontends, inboxes).await
        }
        CmdInternal::Spawn(from_realm_id, target_realm_id, new_realm_hint, target_id) => {
            realm_mngr_inbox
                .send(RealmManagerMsg::CreateNewRealm(
                    from_realm_id,
                    target_realm_id,
                    new_realm_hint,
                    target_id,
                ))
                .await
                .unwrap();
            vec![]
        }
        CmdInternal::AddUserToRealm(realm_id, to_frontend, user_id) => {
            realm_members
                .write()
                .await
                .grant_access(&user_id, &realm_id);
            let sessions_by_user = sessions_by_user.read().await;
            let session_ids = sessions_by_user
                .get(&user_id)
                .map_or(Some(HashSet::new()), |s| Some(s.clone()))
                .unwrap();
            let to_frontends = vec![to_frontend];
            send_to_sessions(session_ids, to_frontends, inboxes).await;

            vec![]
        }
        CmdInternal::Close(_) => todo!(),
    };

    if !stale_sessions.is_empty() {
        let mut inboxes = inboxes.write().await;
        let mut sessions_by_user = sessions_by_user.write().await;
        let mut realm_members = realm_members.write().await;
        for session_id in stale_sessions {
            inboxes.remove(&session_id);
            // todo: make more efficient by storing inverse relationship
            for user in sessions_by_user.iter_mut() {
                let (_, sessions) = user;
                sessions.remove(&session_id);
            }
            realm_members.leave_all_realms(&session_id);
            debug!("Session {:?} removed for being stale", session_id);
        }
    }
}

async fn send_envelope_to_sessions<S>(
    session_ids: S,
    to_frontends: Vec<ToFrontendEnvelope>,
    inboxes: &InboxesBySession,
) -> Vec<SessionId>
where
    S: IntoIterator<Item = SessionId>,
{
    let mut stale_sessions = vec![];
    let inboxes = inboxes.read().await;
    for session_id in session_ids {
        if let Some(inbox) = inboxes.get(&session_id) {
            for to_f in to_frontends.iter() {
                if let Err(e) = inbox.send(to_f.clone()).await {
                    debug!(
                        "Couldn't send to {:?} because of {:?}, assuming stale:",
                        session_id,
                        e.to_string()
                    );
                    stale_sessions.push(session_id.clone());
                }
            }
        } else {
            panic!("Inbox not found");
        }
    }
    stale_sessions
}

async fn send_to_sessions<S>(
    session_ids: S,
    to_frontends: Vec<ToFrontend>,
    inboxes: &InboxesBySession,
) -> Vec<SessionId>
where
    S: IntoIterator<Item = SessionId>,
{
    let in_envelopes = to_frontends
        .into_iter()
        .map(ToFrontendEnvelope::FromRealm)
        .collect();
    send_envelope_to_sessions(session_ids, in_envelopes, inboxes).await
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
                        error!("Client disconnected, stop sending");
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
                RealmThreadMsg::SendJoin(realm_id, user_id, session_id) => {
                    let realm = Realm { id: realm_id };
                    let msg = model.joined(user_id, session_id);
                    if let Some(msg) = msg {
                        let (updated_model, cmd) = model.update(msg, realm);
                        model = updated_model;
                        if let Err(_) = cmd_inbox.send(cmd).await {
                            todo!("Client disconnected, stop sending");
                        }
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
        CmdInternal::None.into()
    }

    pub fn batch<CS>(&self, cmds: CS) -> Cmd
    where
        CS: IntoIterator<Item = Cmd>,
    {
        CmdInternal::Cmds(cmds.into_iter().collect()).into()
    }

    pub fn broadcast<'a, I>(&self, msgs: I) -> Cmd
    where
        I: IntoIterator<Item = ToFrontend>,
    {
        let msgs: Vec<ToFrontend> = msgs.into_iter().collect();
        CmdInternal::BroadcastToRealm(self.id.clone(), msgs).into()
    }

    pub fn to_user<'a, I>(&self, user_id: UserId, msgs: I) -> Cmd
    where
        I: IntoIterator<Item = ToFrontend>,
    {
        let msgs: Vec<ToFrontend> = msgs.into_iter().collect();
        CmdInternal::SendToUser(user_id, msgs).into()
    }

    pub fn to_session<'a, I>(&self, session_id: SessionId, msgs: I) -> Cmd
    where
        I: IntoIterator<Item = ToFrontend>,
    {
        let msgs: Vec<ToFrontend> = msgs.into_iter().collect();
        CmdInternal::SendToSession(session_id, msgs).into()
    }

    pub fn spawn<F>(&self, hint: NewRealmHint, got_new_realm: F) -> Cmd
    where
        F: FnOnce(Realm) -> Msg,
    {
        let id = RealmId::Realm(Uuid::new_v4().to_string());
        let new_realm = Realm { id: id.clone() };
        let to_backend = got_new_realm(new_realm);
        CmdInternal::Spawn(self.id.clone(), id, hint, to_backend).into()
    }

    pub fn add_user<F>(&self, user_id: UserId, entered_realm: F) -> Cmd
    where
        F: FnOnce(RealmId) -> ToFrontend,
    {
        let to_frontend = entered_realm(self.id.clone());
        CmdInternal::AddUserToRealm(self.id.clone(), to_frontend, user_id).into()
    }

    pub fn close(&self) -> Cmd {
        CmdInternal::Close(self.id.clone()).into()
    }
}

impl Realms {
    pub fn new() -> Realms {
        Realms {
            realms: HashMap::new(),
        }
    }

    pub async fn create_realm_with_id(
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
        debug!("Adding new realm: {:?}", id);
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
        self.create_realm_with_id(
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
        self.create_realm_with_id(
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
    CreateNewRealm(RealmId, RealmId, NewRealmHint, Msg),
}
type RealmManagerInbox = Sender<RealmManagerMsg>;

impl AppState {
    pub async fn new() -> Self {
        // Effective domain name.
        let rp_id = "elmcards.axelerator.de";
        // Url containing the effective domain name
        // MUST include the port number!
        let rp_origin = Url::parse("https://elmcards.axelerator.de:8080").expect("Invalid URL");
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
        let realm_members = Arc::new(RwLock::new(RealmMembersStore::default()));
        let sessions_by_user = Arc::new(RwLock::new(HashMap::new()));
        let events_inbox_by_session_id = Arc::new(RwLock::new(HashMap::new()));

        let (realm_mngr_inbox, mut realm_mngr_receiver) = mpsc::channel::<RealmManagerMsg>(32);

        let realms_ = realms.clone();
        let realm_members_ = realm_members.clone();
        let events_inbox_by_session_id_ = events_inbox_by_session_id.clone();
        let sessions_by_user_ = sessions_by_user.clone();
        let realm_mngr_inbox_ = realm_mngr_inbox.clone();
        tokio::spawn(async move {
            while let Some(msg) = realm_mngr_receiver.recv().await {
                match msg {
                    RealmManagerMsg::CreateNewRealm(src_realm_id, target_realm_id, hint, msg) => {
                        debug!("Spawning new realm from {:?} ", src_realm_id);
                        realms_
                            .write()
                            .await
                            .create_realm_with_id(
                                &target_realm_id,
                                realm_members_.clone(),
                                events_inbox_by_session_id_.clone(),
                                sessions_by_user_.clone(),
                                realm_mngr_inbox_.clone(),
                                Some(hint),
                            )
                            .await;
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

    pub async fn register_new_session(
        &self,
        session_id: &SessionId,
        user_id: &UserId,
    ) -> std::result::Result<ReceiverStream<ToFrontendEnvelope>, String> {
        let (inbox, receiver) = mpsc::channel(32);

        let mut event_inboxes = self.events_inbox_by_session_id.write().await;
        event_inboxes.insert(session_id.clone(), inbox);
        drop(event_inboxes);

        let memberships = self.realm_members.read().await.memberships(&user_id);

        for realm_id in memberships {
            self.enter_realm(session_id, user_id, &realm_id)
                .await
                .unwrap();
        }

        Ok(ReceiverStream::new(receiver))
    }

    pub async fn enter_realm(
        &self,
        session_id: &SessionId,
        user_id: &UserId,
        realm_id: &RealmId,
    ) -> std::result::Result<(), String> {
        if !self
            .realm_members
            .read()
            .await
            .is_allowed_to_join(user_id, realm_id)
        {
            return Err("Not a member".to_string());
        }

        self.realm_members
            .write()
            .await
            .enter_realm(user_id, session_id, realm_id);
        self.realms
            .read()
            .await
            .send_joined_msg(realm_id, user_id, session_id)
            .await;
        debug!(
            "User({:?}) with session({:?}) entered '{:?}'",
            user_id, session_id, realm_id
        );
        Ok(())
    }

    pub async fn is_user_member_of(&self, user_id: &UserId, realm_id: &RealmId) -> bool {
        self.realm_members
            .read()
            .await
            .is_user_member_of(user_id, realm_id)
    }

    pub async fn is_session_member_of(&self, session_id: &SessionId, realm_id: &RealmId) -> bool {
        self.realm_members
            .read()
            .await
            .is_session_member_of(session_id, realm_id)
    }

    pub async fn send_from_frontend(
        &self,
        realm_id: RealmId,
        to_backend: ToBackend,
        user_id: UserId,
        session_id: SessionId,
    ) {
        if self.is_user_member_of(&user_id, &realm_id).await {
            self.realms
                .read()
                .await
                .send_from_frontend(realm_id, to_backend, user_id, session_id)
                .await;
        } else {
            tracing::warn!("{:?} not a member of {:?}", session_id, realm_id);
        }
    }

    pub async fn grant_access(&self, user_id: &UserId, realm_id: &RealmId) {
        self.realm_members
            .write()
            .await
            .grant_access(user_id, realm_id);
    }
}
