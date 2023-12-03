use itertools::Itertools;
use std::io::{self, Read};
use std::str::FromStr;

struct ValuesByColor {
    red: u32,
    blue: u32,
    green: u32,
}

struct Game {
    id: u32,
    maxes: ValuesByColor,
}

fn main() -> io::Result<()> {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf)?;

    let games: Vec<Game> = buf
        .lines()
        .map(|l| l.parse().expect("Failed to parse"))
        .collect();

    let part1_maxes = ValuesByColor {
        red: 12,
        green: 13,
        blue: 14,
    };

    let part1: u32 = games
        .iter()
        .filter(|game| {
            game.maxes.red <= part1_maxes.red
                && game.maxes.blue <= part1_maxes.blue
                && game.maxes.green <= part1_maxes.green
        })
        .map(|game| game.id)
        .sum();

    let part2: u32 = games
        .into_iter()
        .map(|game| game.maxes.red * game.maxes.blue * game.maxes.green)
        .sum();

    println!("Part 1: {}\nPart 2: {}", part1, part2);

    Ok(())
}

impl FromStr for Game {
    type Err = Box<dyn std::error::Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (id_str, rounds) = s.splitn(2, ": ").collect_tuple().unwrap();
        let id = id_str
            .trim_start_matches("Game ")
            .parse::<u32>()
            .expect("game id to be valid");

        let maxes = rounds.split("; ").flat_map(|round| round.split(", ")).fold(
            ValuesByColor {
                red: 0,
                blue: 0,
                green: 0,
            },
            |mut acc, draw| {
                let (num, color) = draw.splitn(2, ' ').collect_tuple().unwrap();
                let num = num.parse::<u32>().expect("Invalid draw count");

                match color {
                    "red" => acc.red = num.max(acc.red),
                    "blue" => acc.blue = num.max(acc.blue),
                    "green" => acc.green = num.max(acc.green),
                    _ => panic!("Invalid color"),
                };
                acc
            },
        );

        Ok(Game { id, maxes })
    }
}
