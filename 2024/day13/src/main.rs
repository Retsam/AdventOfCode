use std::collections::{BinaryHeap, HashSet};
use std::error;
use std::io::{self, Read};

use itertools::Itertools;

type Pair = (u64, u64);
#[derive(Debug)]
struct Puzzle {
    a: Pair,
    b: Pair,
    prize: Pair,
}

const PART_2_ADD: u64 = 10000000000000;

// A * 94 + B * 22 = 8400
// A * 34 + B * 67 = 5400

// A = (8400 - B * 22) / 94
// A * 34 + B * 67 = 5400

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let input = buf
        .trim()
        .split("\n\n")
        .map(|s| {
            let (a, b, prize): (&str, &str, &str) = s.splitn(3, "\n").collect_tuple().unwrap();

            let parse_line = |str: &str, prefix: &str| -> Pair {
                str[prefix.len()..]
                    .splitn(2, ", ")
                    .map(|s| s[2..].parse::<u64>().unwrap())
                    .collect_tuple()
                    .unwrap()
            };

            let a = parse_line(a, "Button A: ");
            let b = parse_line(b, "Button B: ");
            let prize = parse_line(prize, "Prize: ");

            Puzzle { a, b, prize }
        })
        .collect_vec();

    let p1 = input
        .iter()
        .map(|puz| search(puz).unwrap_or(0))
        .sum::<u64>();

    let p2 = input
        .iter()
        .map(|puz| {
            math_it(&Puzzle {
                prize: (puz.prize.0 + PART_2_ADD, puz.prize.1 + PART_2_ADD),
                ..*puz
            })
            .unwrap_or(0)
        })
        .sum::<u64>();

    println!("{p1} {p2}");
    Ok(())
}

#[derive(Debug, PartialEq, Eq)]
struct SearchEntry {
    presses: Pair,
}
impl SearchEntry {
    fn cost(&self) -> u64 {
        3 * self.presses.0 + self.presses.1
    }
    fn score(&self, puz: &Puzzle) -> Pair {
        (
            self.presses.0 * puz.a.0 + self.presses.1 * puz.b.0,
            self.presses.0 * puz.a.1 + self.presses.1 * puz.b.1,
        )
    }
}
impl PartialOrd for SearchEntry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for SearchEntry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.cost().cmp(&other.cost()).reverse()
    }
}

fn search(input: &Puzzle) -> Option<u64> {
    let prize = input.prize;

    let mut to_search = BinaryHeap::<SearchEntry>::new();
    let mut visited = HashSet::<Pair>::new();

    to_search.push(SearchEntry { presses: (0, 0) });
    while let Some(search) = to_search.pop() {
        let score = search.score(input);
        // println!("{search:?}, {:?}", score);
        if score == prize {
            return Some(search.cost());
        }
        // probably not needed
        if score.0 != 0 && score.1 != 0 && prize.0 % score.0 == 0 && prize.1 % score.1 == 0 {
            let dx = prize.0 / score.0;
            let dy = prize.1 / score.1;
            if dx == dy {
                return Some(search.cost() * dx);
            }
        }
        if score.0 > prize.0 || score.1 > prize.1 {
            continue;
        }
        if search.presses.0 > 100 || search.presses.1 > 100 {
            continue;
        }
        let mut maybe_add = |pair: Pair| {
            if visited.contains(&pair) {
                return;
            }
            visited.insert(pair);
            to_search.push(SearchEntry { presses: pair });
        };

        maybe_add((search.presses.0 + 1, search.presses.1));
        maybe_add((search.presses.0, search.presses.1 + 1));
    }
    None
}

fn math_it(puz: &Puzzle) -> Option<u64> {
    let Puzzle {
        a: (ax, ay),
        b: (bx, by),
        prize: (px, py),
    } = *puz;
    let a = ((px as f64) / (bx as f64) - (py as f64) / (by as f64))
        / ((ax as f64) / (bx as f64) - (ay as f64) / (by as f64));
    let b = (py as f64 - a * ay as f64) / by as f64;

    let a = a.round() as u64;
    let b = b.round() as u64;
    (a * ax + b * bx == px && a * ay + b * by == py).then_some(a * 3 + b)
}
