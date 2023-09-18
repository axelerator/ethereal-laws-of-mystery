use crate::users::UserId;
use elm_rs::{Elm, ElmDecode, ElmEncode};
use rand::seq::SliceRandom;
use serde::{Deserialize, Serialize};

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub struct GameInfo {
    center: [CardContent; 5],
}

#[derive(Elm, ElmEncode, Deserialize, Debug, Clone)]
pub enum ToGame {
    DrawFromPile,
    Play(usize, usize),
}

#[derive(Elm, ElmEncode, ElmDecode, Serialize, Deserialize, Debug, Clone)]
pub enum Location {
    Deck,
    MyHand(u8),
    DiscardPile,
    InFlight(f32, f32),
    CenterRow(usize),
}

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub enum Transition {
    IDraw(CardContent),
    TheyDraw,
    IPlayed(Location),
}

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
    for number in 0..10 {
        cards.push(CardContent::NumberCard(number));
        cards.push(CardContent::NumberCard(number));
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
}
impl Game {
    pub fn new(user_ids: Vec<UserId>) -> Game {
        let players = user_ids.into_iter().map(Player::new).collect();
        let pile = deck();
        let op = CardContent::OperatorCard(Operator::Plus);
        let eq = CardContent::SwapOperators;
        let n1 = CardContent::NumberCard(1);
        let n2 = CardContent::NumberCard(2);
        let n3 = CardContent::NumberCard(9);
        let center = [n1, op, n2, eq, n3];
        Game {
            players,
            pile,
            center,
        }
    }

    pub fn update(self, msg: ToGame) -> GameChanger {
        match msg {
            ToGame::DrawFromPile => {
                let user = self.players[0].id;
                self.draw(user, 1)
            }
            ToGame::Play(from_hand_pos, to_deck_pos) => {
                let user = self.players[0].id;
                self.play(user, from_hand_pos, to_deck_pos)
            }
        }
    }
    pub fn play(mut self, user: UserId, from_hand_pos: usize, to_deck_pos: usize) -> GameChanger {
        let player = self
            .players
            .iter_mut()
            .find(|p| p.id == user)
            .expect("Player not part of the game");

        let played_card = player.hand.remove(from_hand_pos);
        self.pile[to_deck_pos] = played_card;

        let mut transitions = vec![];
        for player in self.players.iter() {
            if player.id == user {
                transitions.push((user, Transition::IPlayed(Location::CenterRow(to_deck_pos))));
            } else {
                //transitions.push((user, Transition::TheyDraw));
            }
        }
        (self, transitions)
    }

    pub fn draw(mut self, user: UserId, n: usize) -> GameChanger {
        let drawn_cards: Vec<CardContent> = self.pile.drain(0..n).collect();
        let mut transitions = vec![];
        for drawn_card in drawn_cards.iter() {
            for player in self.players.iter() {
                if player.id == user {
                    transitions.push((user, Transition::IDraw(drawn_card.clone())));
                } else {
                    transitions.push((user, Transition::TheyDraw));
                }
            }
        }
        let player = self
            .players
            .iter_mut()
            .find(|p| p.id == user)
            .expect("Player not part of the game");
        player.hand.extend(drawn_cards);

        (self, transitions)
    }

    pub fn game_info(&self) -> GameInfo {
        GameInfo {
            center: self.center.clone(),
        }
    }
}
