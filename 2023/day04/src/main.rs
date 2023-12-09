use itertools::Itertools;
use std::{
    collections::HashSet,
    io::{self, Read},
    str::FromStr,
};

#[derive(Debug)]
struct Card {
    win_count: u32,
}

impl FromStr for Card {
    type Err = String;
    fn from_str(line: &str) -> Result<Self, Self::Err> {
        let (_, cards) = line
            .splitn(2, ": ")
            .collect_tuple::<(&str, &str)>()
            .ok_or("expected ': '")?;
        let (win, has) = cards
            .splitn(2, " | ")
            .map(|nums| {
                nums.split_whitespace()
                    .map(|n| n.parse::<u8>().unwrap())
                    .collect::<HashSet<_>>()
            })
            .collect_tuple()
            .ok_or("??")?;

        let win_count = has.intersection(&win).count() as u32;
        Ok(Card { win_count })
    }
}

fn main() -> Result<(), String> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let cards = buf
        .lines()
        .map(str::parse::<Card>)
        .collect::<Result<Vec<_>, _>>()?;

    let part1: usize = cards
        .iter()
        .map(|x| {
            if x.win_count == 0 {
                0
            } else {
                2_usize.pow(x.win_count - 1)
            }
        })
        .sum();
    println!("{part1}");

    let card_count = cards.len();
    let part2: u32 = cards
        .into_iter()
        .rev()
        .fold(
            Vec::<u32>::with_capacity(card_count),
            |mut vec, Card { win_count }| {
                let score: u32 = 1 + &vec.iter().rev().take(win_count as usize).sum::<u32>();
                vec.push(score);
                vec
            },
        )
        .iter()
        .sum();
    println!("{part2}");

    Ok(())
}
