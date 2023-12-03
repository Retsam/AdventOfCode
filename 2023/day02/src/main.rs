use std::io::{self, Read};

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

    let games: Vec<_> = buf.lines().map(parse_game).collect();

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

fn split_at_match<'a>(input: &'a str, pattern: &str) -> (&'a str, &'a str) {
    let (idx, _) = input
        .match_indices(pattern)
        .next()
        .unwrap_or_else(|| panic!("Failed to match pattern {}", pattern));
    let (a, b) = input.split_at(idx);
    (a, &b[pattern.len()..])
}

fn parse_game(input: &str) -> Game {
    let (game_id_str, rounds) = split_at_match(input, ": ");

    let id = game_id_str
        .chars()
        .skip("Game ".len())
        .collect::<String>()
        .parse::<u32>()
        .expect("game id to be valid");

    let maxes = rounds.split("; ").flat_map(|round| round.split(", ")).fold(
        ValuesByColor {
            red: 0,
            blue: 0,
            green: 0,
        },
        |mut acc, draw| {
            let (num_str, color) = split_at_match(draw, " ");
            let num = num_str.parse::<u32>().expect("Invalid draw count");

            match color {
                "red" => acc.red = num.max(acc.red),
                "blue" => acc.blue = num.max(acc.blue),
                "green" => acc.green = num.max(acc.green),
                _ => panic!("Invalid color"),
            };
            acc
        },
    );

    Game { id, maxes }
}
