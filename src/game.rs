use crate::users::UserId;
use elm_rs::{Elm, ElmDecode, ElmEncode};
use serde::{Deserialize, Serialize};
use rand::seq::SliceRandom;


#[derive(Elm, ElmEncode, Deserialize, Debug, Clone)]
pub enum ToGame {
    DrawFromPile,
}

#[derive(Elm, ElmDecode, Serialize, Debug, Clone)]
pub enum Transition {
    IDraw(CardContent),
    TheyDraw,
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
    SwapOperators
}

pub struct Game {
    players: Vec<Player>,
    pile: Vec<CardContent>,
    discard: Vec<CardContent>,
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
type PlayerId = usize;

impl Game {
    pub fn new(user_ids: Vec<UserId>) -> Game {
        let players = user_ids.into_iter().map(Player::new).collect();
        let pile = deck();
        let discard = vec![];
        Game {
            players,
            pile,
            discard,
        }
    }

    pub fn update(self, msg: ToGame) -> GameChanger {
        match msg {
            ToGame::DrawFromPile => {
                let user = self.players[0].id;
                self.draw(user, 1)
            }
        }
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
}
