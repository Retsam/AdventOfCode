use itertools::Itertools;
use std::error;
use std::io::{self, Read};

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let (fresh_ranges_str, available_str) = buf.split_once("\n\n").unwrap();

    let mut fresh_ranges = fresh_ranges_str
        .lines()
        .map(|line| {
            let (start, end) = line.split_once('-').unwrap();
            start.parse::<u64>().unwrap()..=end.parse::<u64>().unwrap()
        })
        .collect_vec();

    let p1 = available_str
        .lines()
        .filter(|line| {
            let num = line.parse::<u64>().unwrap();
            fresh_ranges.iter().any(|range| range.contains(&num))
        })
        .count() as u64;

    fresh_ranges.sort_by(|x, y| x.start().cmp(y.start()));

    let mut final_ranges = vec![];
    let mut iter = fresh_ranges.into_iter().peekable();
    while let Some(mut current) = iter.next() {
        loop {
            let next = iter.peek();
            if let Some(next_range) = next.filter(|n| current.contains(n.start())) {
                let new_end = *next_range.end().max(current.end());
                current = *current.start()..=new_end;
                iter.next();
            } else {
                final_ranges.push(current);
                break;
            }
        }
    }
    let p2 = final_ranges
        .iter()
        .map(|r| r.end() - r.start() + 1)
        .sum::<u64>();

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}
