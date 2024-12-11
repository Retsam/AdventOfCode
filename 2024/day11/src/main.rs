use std::collections::HashMap;
use std::error;
use std::io::{self, Read};

use itertools::Itertools;

type Stone = u64;
fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buffer = String::new();
    io::stdin()
        .read_to_string(&mut buffer)
        .map_err(|_| "Failed to read input")?;

    let init = buffer
        .trim()
        .split(" ")
        .map(|stone| stone.parse::<Stone>().unwrap())
        .collect_vec();

    let answers = &mut HashMap::new();
    let part1 = simulate_generation(&init, 25, answers);
    let part2 = simulate_generation(&init, 75, answers);
    println!("Part 1: {part1}\nPart 2: {part2}");
    Ok(())
}

fn step_stone(stone: Stone) -> Vec<Stone> {
    if stone == 0 {
        return vec![1];
    }

    let stone_str = stone.to_string();

    if stone_str.len() % 2 == 0 {
        let (a, b) = stone_str.split_at(stone_str.len() / 2);
        vec![a.parse().unwrap(), b.parse().unwrap()]
    } else {
        vec![stone * 2024]
    }
}
type Generation = u32;
fn simulate_generation(
    stones: &[Stone],
    remaining: Generation,
    answers: &mut HashMap<(Stone, Generation), u64>,
) -> u64 {
    stones
        .iter()
        .map(|&stone| {
            if let Some(answer) = answers.get(&(stone, remaining)) {
                *answer
            } else {
                let answer = simulate_stone(&stone, remaining, answers);
                answers.insert((stone, remaining), answer);
                answer
            }
        })
        .sum()
}

fn simulate_stone(
    stone: &Stone,
    remaining: Generation,
    answers: &mut HashMap<(Stone, Generation), u64>,
) -> u64 {
    let next = step_stone(*stone);
    if remaining == 1 {
        return next.len() as u64;
    }

    simulate_generation(&next, remaining - 1, answers)
}
