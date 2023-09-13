use crate::users::UserId;

pub enum Move {
    DrawFromPile,
}

pub enum Transition {
    IDraw(Card),
    TheyDraw,
}

pub struct Player {
    id: UserId,
    hand: Vec<Card>,
}

impl Player {
    pub fn new(id: UserId) -> Player {
        Player { id, hand: vec![] }
    }
}

#[derive(Debug, Clone)]
enum Suite {
    Red,
    Black,
}

#[derive(Debug, Clone)]
struct Card {
    number: u8,
    suite: Suite,
}

pub struct Game {
    players: Vec<Player>,
    pile: Vec<Card>,
    discard: Vec<Card>,
}

fn deck() -> Vec<Card> {
    let mut cards = vec![];
    for number in 1..10 {
        cards.push(Card {
            suite: Suite::Red,
            number,
        });
        cards.push(Card {
            suite: Suite::Black,
            number,
        });
    }
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

    pub fn draw(mut self, user: UserId, n: usize) -> GameChanger {
        let drawn_cards: Vec<Card> = self.pile.drain(0..n).collect();
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
