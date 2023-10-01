use std::collections::HashSet;

use crate::{
    hades::RealmId,
    users::{UserId, Users},
};
use elm_rs::{Elm, ElmDecode, ElmEncode};
use rand::seq::SliceRandom;
use serde::{Deserialize, Serialize};
use tokio::sync::Mutex;
use tracing::{debug, error};

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub struct GameInfo {
    pile_size: usize,
    discard_pile: Vec<CardContent>,
    center: [CardContent; 5],
    hand: Vec<CardContent>,
    game_state: GameState,
    opponents: Vec<Opponent>,
    turn: RelativeOpponent,
}

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub struct Opponent {
    name: String,
    hand_size: usize,
}

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub enum GameState {
    Running,
    GameOver(bool),
}

#[derive(Elm, ElmEncode, Deserialize, Debug, Clone)]
pub enum ToGame {
    DrawFromPile,
    Play(usize, usize),
    LeaveGame,
    RestartGame,
}

type HandPosition = usize;

#[derive(Elm, ElmEncode, ElmDecode, Serialize, Deserialize, Debug, Clone)]
pub enum Location {
    Deck,
    MyHand(HandPosition),
    TheirHand(RelativeOpponent, HandPosition),
    DiscardPile(usize),
    InFlight(f32, f32),
    InFlightOpen(f32, f32),
    CenterRow(usize),
}

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub enum Transition {
    IDraw(CardContent),
    TheyDraw(RelativeOpponent, RelativeOpponent),
    IPlayed(Location),
    TheyPlayed(
        RelativeOpponent,
        Location,
        Location,
        CardContent,
        RelativeOpponent,
    ),
    IWon,
    ILost,
    TurnChanged(RelativeOpponent),
    GameEnded(RealmId),
}

pub type RelativeOpponent = usize;

pub struct Player {
    id: UserId,
    name: Option<String>,
    hand: Vec<CardContent>,
}

impl Player {
    pub fn new(id: UserId) -> Player {
        Player {
            id,
            hand: vec![],
            name: None,
        }
    }
}

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Times,
}

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub enum CardContent {
    NumberCard(i8),
    OperatorCard(Operator),
    SwapOperators,
}

fn deck() -> Vec<CardContent> {
    let mut cards = vec![];
    for number in 1..10 {
        cards.push(CardContent::NumberCard(number));
        cards.push(CardContent::NumberCard(number));
        cards.push(CardContent::NumberCard(number));
        cards.push(CardContent::NumberCard(number));
    }

    for _ in 0..4 {
        cards.push(CardContent::OperatorCard(Operator::Plus));
        cards.push(CardContent::OperatorCard(Operator::Minus));
        cards.push(CardContent::OperatorCard(Operator::Times));
        cards.push(CardContent::SwapOperators);
    }

    let mut rng = rand::thread_rng();
    cards.shuffle(&mut rng);
    cards
}

type GameChanger = (Game, Vec<(UserId, Transition)>);

enum MoveType {
    JustPlace,
    SwapOperators,
}

pub struct Game {
    players: Vec<Player>,
    pile: Vec<CardContent>,
    discard_pile: Vec<CardContent>,
    center: [CardContent; 5],
    winner: Option<UserId>,
    voted_for_restart: HashSet<UserId>,
    current_player: UserId,
}
impl Game {
    pub fn new(user_ids: Vec<UserId>) -> Game {
        let current_player = user_ids[0];
        let mut players: Vec<Player> = user_ids.into_iter().map(Player::new).collect();
        let mut pile = deck();
        let op = CardContent::OperatorCard(Operator::Plus);
        let eq = CardContent::SwapOperators;
        let n1 = CardContent::NumberCard(1);
        let n2 = CardContent::NumberCard(2);
        let n3 = CardContent::NumberCard(9);
        let center = [n1, op, n2, eq, n3];
        let voted_for_restart = HashSet::new();
        for player in players.iter_mut() {
            for _ in 0..4 {
                let card = pile.pop().unwrap();
                player.hand.push(card);
            }
        }
        Game {
            players,
            pile,
            center,
            winner: None,
            voted_for_restart,
            current_player,
            discard_pile: vec![],
        }
    }

    pub fn update(self, msg: ToGame, user_id: &UserId) -> GameChanger {
        self.players
            .iter()
            .find(|p| &p.id == user_id)
            .expect("Player not part of the game");

        match msg {
            ToGame::DrawFromPile => self.draw(user_id, 1),
            ToGame::Play(from_hand_pos, to_deck_pos) => {
                self.play(user_id, from_hand_pos, to_deck_pos)
            }
            ToGame::LeaveGame => (self, vec![]),
            ToGame::RestartGame => self.restart(user_id),
        }
    }

    pub fn is_over(&self) -> bool {
        match self.winner {
            Some(_) => true,
            None => false,
        }
    }

    fn play(mut self, user: &UserId, from_hand_pos: usize, to_deck_pos: usize) -> GameChanger {
        match self.validate(user, from_hand_pos, to_deck_pos) {
            Err(msg) => {
                error!(msg);
                return (self, vec![]);
            }
            Ok(MoveType::JustPlace) => {
                let player = self.players.iter_mut().find(|p| &p.id == user).unwrap();

                let played_card = player.hand.remove(from_hand_pos);

                let target_location = Location::CenterRow(to_deck_pos);
                let next_player_id = self.proceed_to_next_player();

                let mut transitions = vec![];
                for other_player in self.players.iter() {
                    if &other_player.id == user {
                        transitions.push((
                            other_player.id,
                            Transition::IPlayed(target_location.clone()),
                        ));
                    } else {
                        let opponent = self.player_relative_to(&other_player.id, user);
                        let from = Location::TheirHand(opponent, from_hand_pos);
                        let to = target_location.clone();
                        transitions.push((
                            other_player.id,
                            Transition::TheyPlayed(
                                self.player_relative_to(&other_player.id, user),
                                from,
                                to,
                                played_card.clone(),
                                self.player_relative_to(&other_player.id, &next_player_id),
                            ),
                        ));
                    }
                }
                println!(
                    "Played card: {:?} to deck pos: {:?}",
                    played_card, to_deck_pos
                );
                let discarded_card = std::mem::replace(&mut self.center[to_deck_pos], played_card);
                self.discard_pile.push(discarded_card);

                if let Some(game_over_transition) = self.is_game_over(user) {
                    transitions.extend(game_over_transition);
                    self.winner = Some(*user);
                }
                (self, transitions)
            }
            Ok(MoveType::SwapOperators) => {
                let player = self.players.iter_mut().find(|p| &p.id == user).unwrap();
                let played_card = player.hand.remove(from_hand_pos);

                let target_location = Location::CenterRow(to_deck_pos);
                let next_player_id = self.proceed_to_next_player();

                let mut transitions = vec![];
                for other_player in self.players.iter() {
                    if &other_player.id == user {
                        transitions.push((
                            other_player.id,
                            Transition::IPlayed(target_location.clone()),
                        ));
                    } else {
                        let opponent = self.player_relative_to(&other_player.id, user);
                        let from = Location::TheirHand(opponent, from_hand_pos);
                        let to = target_location.clone();
                        transitions.push((
                            other_player.id,
                            Transition::TheyPlayed(
                                self.player_relative_to(&other_player.id, user),
                                from,
                                to,
                                played_card.clone(),
                                self.player_relative_to(&other_player.id, &next_player_id),
                            ),
                        ));
                    }
                }
                let replaced_operator =
                    std::mem::replace(&mut self.center[to_deck_pos], played_card);
                let old_equal_pos = if to_deck_pos == 1 { 3 } else { 1 };
                let replaced_equal =
                    std::mem::replace(&mut self.center[old_equal_pos], replaced_operator);
                self.discard_pile.push(replaced_equal);

                if let Some(game_over_transition) = self.is_game_over(user) {
                    transitions.extend(game_over_transition);
                    self.winner = Some(*user);
                }
                (self, transitions)
            }
        }
    }

    fn validate(
        &self,
        user: &UserId,
        from_hand_pos: usize,
        to_deck_pos: usize,
    ) -> Result<MoveType, &str> {
        let player = self
            .find_player(user)
            .ok_or("User is not a player in this game")?;

        if user != &self.current_player {
            return Err("It's not this players turn");
        }

        let hand_card = player.hand.get(from_hand_pos).ok_or("Not in hand")?;
        match hand_card {
            CardContent::NumberCard(_) => {
                if to_deck_pos == 1 || to_deck_pos == 3 {
                    return Err("Can't place a number card there");
                }
            }
            CardContent::SwapOperators => {
                if self.is_equal_left() && to_deck_pos != 3 {
                    return Err("Swap can only be placed on 3");
                }
                if !self.is_equal_left() && to_deck_pos != 1 {
                    return Err("Swap can only be placed on 1");
                }
                return Ok(MoveType::SwapOperators);
            }
            CardContent::OperatorCard(_) => {
                if self.is_equal_left() && to_deck_pos != 3 {
                    return Err("Can't place a operator card there");
                }
                if !self.is_equal_left() && to_deck_pos != 1 {
                    return Err("Can't place operator there");
                }
            }
        };
        Ok(MoveType::JustPlace)
    }

    fn find_player(&self, user: &UserId) -> Option<&Player> {
        self.players.iter().find(|p| &p.id == user)
    }

    fn draw(mut self, user: &UserId, n: usize) -> GameChanger {
        if user != &self.current_player {
            error!("It's not this players turn");
            return (self, vec![]);
        }
        let drawn_cards: Vec<CardContent> = self.pile.drain(0..n).collect();
        let mut transitions = vec![];
        let next_player_id = self.proceed_to_next_player();
        println!(
            "After draw of {:?} next player is {:?}",
            user, next_player_id
        );
        for drawn_card in drawn_cards.iter() {
            for player in self.players.iter() {
                if &player.id == user {
                    transitions.push((*user, Transition::IDraw(drawn_card.clone())));
                } else {
                    transitions.push((
                        player.id,
                        Transition::TheyDraw(
                            self.player_relative_to(user, &player.id),
                            self.player_relative_to(&player.id, &next_player_id),
                        ),
                    ));
                }
            }
        }
        let player = self
            .players
            .iter_mut()
            .find(|p| &p.id == user)
            .expect("Player not part of the game");
        player.hand.extend(drawn_cards);

        (self, transitions)
    }

    fn proceed_to_next_player(&mut self) -> UserId {
        let my_pos = self
            .players
            .iter()
            .position(|p| p.id == self.current_player)
            .unwrap();
        let next_op_pos = if my_pos == self.players.len() - 1 {
            0
        } else {
            my_pos + 1
        };
        let next_player_id = self.players[next_op_pos].id;
        self.current_player = next_player_id;
        debug!("It's now {} turn", self.current_player);
        next_player_id
    }

    pub fn game_info(&self, player_id: UserId) -> GameInfo {
        let player = self
            .players
            .iter()
            .find(|p| p.id == player_id)
            .expect("Player not part of the game");

        let hand = player.hand.clone();
        let pile_size = self.pile.len();
        let discard_pile = self.discard_pile.clone();
        let game_state = match self.winner {
            None => GameState::Running,
            Some(winner) => GameState::GameOver(winner == player_id),
        };
        let my_pos = self.players.iter().position(|p| p.id == player_id).unwrap();
        let mut opponents = self.players.iter().collect::<Vec<&Player>>();
        opponents.rotate_left(my_pos);
        let opponents = opponents
            .into_iter()
            .skip(1)
            .map(|p| Opponent {
                name: p.name.as_ref().map(|s| s.clone()).unwrap_or("".to_string()),
                hand_size: p.hand.len(),
            })
            .collect();

        GameInfo {
            center: self.center.clone(),
            hand,
            pile_size,
            discard_pile,
            game_state,
            opponents,
            turn: self.player_relative_to(&self.current_player, &player_id),
        }
    }

    fn player_relative_to(&self, current_player: &UserId, player_id: &UserId) -> RelativeOpponent {
        let my_pos = self
            .players
            .iter()
            .position(|p| &p.id == player_id)
            .unwrap();
        let mut opponents = self.players.iter().collect::<Vec<&Player>>();
        opponents.rotate_left(my_pos);
        let r = opponents
            .iter()
            .position(|p| &p.id == current_player)
            .unwrap();
        println!(
            "{:?}\ncurrent:{:?}\nother:{:?} -> op: {:?}",
            self.players.iter().map(|p| p.id).collect::<Vec<UserId>>(),
            current_player,
            player_id,
            r
        );
        r
    }

    fn restart(mut self, user: &UserId) -> GameChanger {
        self.voted_for_restart.insert(*user);
        (self, vec![])
    }

    fn is_game_over(&self, user_id: &UserId) -> Option<Vec<(UserId, Transition)>> {
        if self.numbers_match() {
            let mut transitions = vec![];
            for player in self.players.iter() {
                if &player.id == user_id {
                    transitions.push((player.id, Transition::IWon));
                } else {
                    transitions.push((player.id, Transition::ILost));
                }
            }
            Some(transitions)
        } else {
            None
        }
    }

    fn numbers_match(&self) -> bool {
        println!("Center: {:?}", self.center);
        let (operator, op1, op2, exp) = if self.is_equal_left() {
            (&self.center[3], 2, 4, 0)
        } else {
            (&self.center[1], 0, 2, 4)
        };
        println!("center: {:?}", self.center);
        let v1 = self.get_val_at(op1);
        let v2 = self.get_val_at(op2);
        let expected = self.get_val_at(exp);
        let actual = match operator {
            CardContent::OperatorCard(Operator::Plus) => v1 + v2,
            CardContent::OperatorCard(Operator::Minus) => v1 - v2,
            CardContent::OperatorCard(Operator::Times) => v1 * v2,
            CardContent::NumberCard(_) => panic!(),
            CardContent::SwapOperators => panic!(),
        };
        println!(
            "{}({}) + {}({}) = {} (exp: {})",
            op1, v1, op2, v2, actual, expected
        );
        actual == expected
    }

    fn get_val_at(&self, pos: usize) -> i8 {
        match self.center[pos] {
            CardContent::SwapOperators => panic!(),
            CardContent::OperatorCard(_) => panic!(),
            CardContent::NumberCard(u) => u,
        }
    }

    fn is_equal_left(&self) -> bool {
        match self.center[1] {
            CardContent::SwapOperators => true,
            _ => false,
        }
    }

    pub(crate) async fn resolve_player_names(mut self, users: &Mutex<Users>) -> Game {
        if self.players[0].name.is_some() {
            return self; // already resolved
        }
        let user_ids = self.players.iter().map(|u| u.id).collect();
        let users = users.lock().await;
        let users = users.by_ids(user_ids).await;
        for player in self.players.iter_mut() {
            if let Some(user) = users.iter().find(|u| u.id == player.id) {
                player.name = Some(user.name.clone());
            }
        }
        self
    }
}
