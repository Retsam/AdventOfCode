use std::error;
use std::{
    io::{self, Read},
    num::ParseIntError,
};

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;
    let input = buf
        .lines()
        .map(parse_input)
        .collect::<Result<Vec<_>, _>>()?;
    let part1: i32 = input
        .iter()
        .map(|line| solve(line, false).expect("Failed"))
        .sum();

    let part2: i32 = input
        .iter()
        .map(|line| solve(line, true).expect("Failed"))
        .sum();

    println!("{part1}");
    println!("{part2}");

    Ok(())
}

fn parse_input(input: &str) -> Result<Vec<i32>, ParseIntError> {
    input.split(' ').map(str::parse).collect()
}

fn solve(line: &[i32], is_part_two: bool) -> Option<i32> {
    let first = *line.first()?;
    if line.iter().all(|&y| y == first) {
        return Some(first);
    }
    let last = *line.last().unwrap();

    let mut prev = first;
    let deltas = line
        .iter()
        .skip(1)
        .map(|x| {
            let r = x - prev;
            prev = *x;
            r
        })
        .collect::<Vec<_>>();
    solve(&deltas, is_part_two).map(|new_val| {
        if is_part_two {
            first - new_val
        } else {
            last + new_val
        }
    })
}
