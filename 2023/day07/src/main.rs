use std::{
    cmp::Ordering,
    collections::HashMap,
    io::{self, Read},
};

fn main() -> Result<(), String> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let input = parse_input(&buf).expect("Failed to parse");
    let part1 = NormalHandEvaluator.evaluate_cards(&input);
    println!("{part1}");
    let part2 = JokerHandEvaluator.evaluate_cards(&input);
    println!("{part2}");

    Ok(())
}

type Card = char;
type Counts = HashMap<Card, u8>;

type Hand = String;

#[derive(Debug)]
struct HandWithBid {
    hand: Hand,
    bid: u32,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
enum HandRank {
    HighCard,
    Pair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}
use HandRank::*;

trait HandEvaluator {
    fn card_order(&self) -> &'static str;
    fn generate_counts(&self, hand: &Hand) -> Counts;

    fn evaluate_cards(&self, hands: &[HandWithBid]) -> u32 {
        let mut ranked_hands = hands
            .iter()
            .map(|x| (x, self.rank(&x.hand)))
            .collect::<Vec<_>>();
        ranked_hands.sort_by(|(a, rank_a), (b, rank_b)| {
            rank_a
                .cmp(rank_b)
                .then(self.compare_equal_ranks(&a.hand, &b.hand))
        });

        ranked_hands
            .into_iter()
            .map(|(HandWithBid { bid, .. }, _)| bid)
            .enumerate()
            .map(|(i, bid)| (i as u32 + 1) * bid)
            .sum()
    }

    fn rank(&self, hand: &Hand) -> HandRank {
        let counts = self.generate_counts(hand);
        let groups_of_size =
            |size| -> usize { counts.values().filter(|&&count| count == size).count() };
        let pair_count = groups_of_size(2);
        if groups_of_size(5) > 0 {
            FiveOfAKind
        } else if groups_of_size(4) > 0 {
            FourOfAKind
        } else if groups_of_size(3) > 0 {
            if pair_count == 1 {
                FullHouse
            } else {
                ThreeOfAKind
            }
        } else {
            match pair_count {
                2 => TwoPair,
                1 => Pair,
                0 => HighCard,
                _ => panic!("Unexpected pair count"),
            }
        }
    }

    fn compare_equal_ranks(&self, hand: &Hand, other: &Hand) -> Ordering {
        let card_order = self.card_order();
        let compare_card = |a, b| {
            let a_idx = card_order.chars().position(|x| x == a);
            let b_idx = card_order.chars().position(|x| x == b);
            a_idx.cmp(&b_idx).reverse() // earlier in order is better
        };
        hand.chars()
            .zip(other.chars())
            .find_map(|(a, b)| match compare_card(a, b) {
                Ordering::Equal => None,
                ord => Some(ord),
            })
            .unwrap_or(Ordering::Equal)
    }
}

struct NormalHandEvaluator;

const PART_1_ORDER: &str = "AKQJT98765432";
impl HandEvaluator for NormalHandEvaluator {
    fn card_order(&self) -> &'static str {
        PART_1_ORDER
    }

    fn generate_counts(&self, hand: &Hand) -> Counts {
        hand.chars().fold(Counts::new(), |mut map, char| {
            *map.entry(char).or_insert(0) += 1;
            map
        })
    }
}

struct JokerHandEvaluator;
const PART_2_ORDER: &str = "AKQT98765432J";
impl HandEvaluator for JokerHandEvaluator {
    fn card_order(&self) -> &'static str {
        PART_2_ORDER
    }

    fn generate_counts(&self, hand: &Hand) -> Counts {
        let mut counts = NormalHandEvaluator.generate_counts(hand);
        if let Some(joker_count) = counts.remove(&'J') {
            match counts.values_mut().max() {
                Some(max) => *max += joker_count,
                // If we don't find a max, then counts is empty, so the hand was 'JJJJJ'
                None => {
                    counts.insert('J', 5);
                }
            }
        }
        counts
    }
}

fn parse_input(input: &str) -> Option<Vec<HandWithBid>> {
    input
        .lines()
        .map(|line| {
            let mut parts = line.split(' ');
            let hand = parts.next()?.to_string();
            let bid = parts.next()?.parse::<u32>().ok()?;
            Some(HandWithBid { hand, bid })
        })
        .collect()
}
