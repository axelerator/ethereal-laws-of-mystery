use std::collections::HashSet;

use crate::users::UserId;
use elm_rs::{Elm, ElmDecode, ElmEncode};
use rand::seq::SliceRandom;
use serde::{Deserialize, Serialize};

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub struct GameInfo {
    pile_size: usize,
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
    DiscardPile,
    InFlight(f32, f32),
    InFlightOpen(f32, f32),
    CenterRow(usize),
}

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub enum Transition {
    IDraw(CardContent),
    TheyDraw(RelativeOpponent),
    IPlayed(Location),
    TheyPlayed(Location, Location, CardContent),
    IWon,
    ILost,
    TurnChanged(RelativeOpponent),
}

type RelativeOpponent = usize;

pub struct Player {
    id: UserId,
    hand: Vec<CardContent>,
}

impl Player {
    pub fn new(id: UserId) -> Player {
        Player { id, hand: vec![] }
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
    NumberCard(u8),
    OperatorCard(Operator),
    SwapOperators,
}

fn deck() -> Vec<CardContent> {
    let mut cards = vec![];
    for number in 1..10 {
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

pub struct Game {
    players: Vec<Player>,
    pile: Vec<CardContent>,
    center: [CardContent; 5],
    winner: Option<UserId>,
    voted_for_restart: HashSet<UserId>,
    current_player: UserId,
}
impl Game {
    pub fn new(user_ids: Vec<UserId>) -> Game {
        let current_player = user_ids[0].clone();
        let mut players : Vec<Player> = user_ids.into_iter().map(Player::new).collect();
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

    fn play(mut self, user: &UserId, from_hand_pos: usize, to_deck_pos: usize) -> GameChanger {
        let player = self
            .players
            .iter_mut()
            .find(|p| &p.id == user)
            .expect("Player not part of the game");

        let played_card = player.hand.remove(from_hand_pos);

        let target_location = Location::CenterRow(to_deck_pos);
        let mut transitions = vec![];
        for other_player in self.players.iter() {
            if &other_player.id == user {
                transitions.push((
                    other_player.id.clone(),
                    Transition::IPlayed(target_location.clone()),
                ));
            } else {
                let opponent = self.player_relative_to(&other_player.id, user);
                let from = Location::TheirHand(opponent, from_hand_pos);
                let to = target_location.clone();
                transitions.push((
                    other_player.id.clone(),
                    Transition::TheyPlayed(from, to, played_card.clone()),
                ));
            }
        }
        println!(
            "Played card: {:?} to deck pos: {:?}",
            played_card, to_deck_pos
        );
        self.center[to_deck_pos] = played_card;

        if let Some(game_over_transition) = self.is_game_over(&user) {
            transitions.extend(game_over_transition);
            self.winner = Some(user.clone());
        }
        (self, transitions)
    }

    fn draw(mut self, user: &UserId, n: usize) -> GameChanger {
        let drawn_cards: Vec<CardContent> = self.pile.drain(0..n).collect();
        let mut transitions = vec![];
        for drawn_card in drawn_cards.iter() {
            for player in self.players.iter() {
                if &player.id == user {
                    transitions.push((user.clone(), Transition::IDraw(drawn_card.clone())));
                } else {
                    transitions.push((
                        player.id.clone(),
                        Transition::TheyDraw(self.player_relative_to(user, &player.id)),
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

    pub fn game_info(&self, player_id: UserId) -> GameInfo {
        let player = self
            .players
            .iter()
            .find(|p| p.id == player_id)
            .expect("Player not part of the game");

        let hand = player.hand.clone();
        let pile_size = self.pile.len();
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
                name: p.id.to_string(),
                hand_size: p.hand.len(),
            })
            .collect();

        GameInfo {
            center: self.center.clone(),
            hand,
            pile_size,
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
        opponents
            .iter()
            .position(|p| &p.id == current_player)
            .unwrap()
    }

    fn restart(mut self, user: &UserId) -> GameChanger {
        self.voted_for_restart.insert(user.clone());
        (self, vec![])
    }

    fn is_game_over(&self, user_id: &UserId) -> Option<Vec<(UserId, Transition)>> {
        if self.numbers_match() {
            let mut transitions =  vec![];
            for player in self.players.iter() {
                if &player.id == user_id {
                    transitions.push((player.id.clone(), Transition::IWon));
                } else {
                    transitions.push((player.id.clone(), Transition::ILost));
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
            (&self.center[3], 2, 3, 0)
        } else {
            (&self.center[1], 0, 2, 4)
        };
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

    fn get_val_at(&self, pos: usize) -> u8 {
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
}
