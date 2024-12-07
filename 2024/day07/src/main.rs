use itertools::Itertools;
use std::io::{self, Read};
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
        .filter(|(target, vals)| test(*target, vals[0], &vals[1..]))
        .map(|(target, _)| target)
        .sum();
    println!("Part 1: {}", part1);
    Ok(())
}

fn test(target: u64, current: u64, vals: &[u64]) -> bool {
    if current > target {
        return false;
    }
    if vals.is_empty() {
        return current == target;
    }
    let next = vals[0];
    let concat = format!("{current}{next}")
        .parse()
        .map(|new_val| test(target, new_val, &vals[1..]))
        .unwrap_or(false);
    concat || test(target, current * next, &vals[1..]) || test(target, current + next, &vals[1..])
}
