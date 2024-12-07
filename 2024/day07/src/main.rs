use itertools::Itertools;
use std::io::{self, Read};

#[derive(Clone, Copy, PartialEq)]
enum Part {
    One,
    Two,
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let input = buffer
        .lines()
        .map(|line| {
            let (lhs, rhs) = line.splitn(2, ": ").collect_tuple().unwrap();
            let vals = rhs
                .split(" ")
                .map(|x| x.parse::<u64>().unwrap())
                .collect_vec();
            (lhs.parse::<u64>().unwrap(), vals)
        })
        .collect_vec();

    let part1: u64 = input
        .iter()
        .filter(|(target, vals)| insert_opr(*target, vals[0], &vals[1..], Part::One))
        .map(|(target, _)| target)
        .sum();
    let part2: u64 = input
        .iter()
        .filter(|(target, vals)| insert_opr(*target, vals[0], &vals[1..], Part::Two))
        .map(|(target, _)| target)
        .sum();
    println!("Part 1: {part1}\nPart 2: {part2}");
    Ok(())
}

fn insert_opr(target: u64, current: u64, vals: &[u64], part: Part) -> bool {
    if current > target {
        return false;
    }
    if vals.is_empty() {
        return current == target;
    }
    let next = vals[0];
    let recurse = |new_val| insert_opr(target, new_val, &vals[1..], part);
    let concat = part == Part::Two
        && format!("{current}{next}")
            .parse()
            .map(recurse)
            .unwrap_or(false);
    concat || recurse(current * next) || recurse(current + next)
}
