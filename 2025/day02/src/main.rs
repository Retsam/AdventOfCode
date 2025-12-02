use itertools::Itertools;
use std::collections::HashSet;
use std::error;
use std::io::{self, Read};
use std::ops::RangeInclusive;

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;
    if buf.is_empty() {
        return Err("Input is empty".into());
    }

    let ranges = buf
        .split(",")
        .map(|s| {
            s.splitn(2, '-')
                .map(|n| n.parse::<u64>().unwrap())
                .collect_tuple::<(u64, u64)>()
                .unwrap()
        })
        .collect::<Vec<_>>();

    let (mut p1, mut p2) = (0, 0);

    for (lower, upper) in ranges {
        // Collect candidates in a set - we can't double count something like 222222 as both a double and a triple (and a 6-tuple)
        let mut candidates = solve(lower, upper, 2);
        p1 += candidates.iter().sum::<u64>();

        for size in 3..=upper.to_string().len() {
            candidates.extend(solve(lower, upper, size));
        }
        p2 += candidates.iter().sum::<u64>();
    }

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}

fn repeat(s: u64, times: usize) -> u64 {
    std::iter::repeat(s.to_string())
        .take(times)
        .collect::<Vec<_>>()
        .join("")
        .parse()
        .unwrap()
}

// Calcluates the possible range of starting numbers to repeat.
// Examples:
// 60-89, size=2 -> [6, 8]
// 69-80, size=2 -> [7, 7] - 6 was eliminated because 66 < 69, 8 was eliminated because 88 > 80
// 88-130, size=2 -> [8, 9]  - stops before going to three digits because size is 2
// 600-1200, size=2 -> [10, 12] - ignores 600-999 because no two-digit repeated numbers can fit there
fn calculate_bounds(lower: u64, upper: u64, size: usize) -> RangeInclusive<u64> {
    let lower_str = lower.to_string();
    let upper_str = upper.to_string();
    let lower_len = lower_str.len();
    let upper_len = upper_str.len();

    let in_range = |candidate| (lower..=upper).contains(&candidate);

    // Early return if neither bound is a multiple of size
    // (The input does not include ranges across more than one order of magnitude, e.g. 99-101 is possible, but 99-1010 is not)
    // If not for this, we'd have to consider the case where some length *between* upper_len and lower_len is a multiple
    if lower_len % size != 0 && upper_len % size != 0 {
        // Intentionally empty range
        return 2..=1;
    }

    let mut lower_bound = if lower_len % size == 0 {
        let start = &lower_str[0..(lower_len / size)];
        start.parse::<u64>().unwrap()
    } else {
        10u64.pow(upper_len as u32 / size as u32 - 1)
    };
    let mut upper_bound = if upper_len % size == 0 {
        let start = &upper_str[0..(upper_len / size)];
        start.parse::<u64>().unwrap()
    } else {
        10u64.pow(lower_len as u32 / size as u32) - 1
    };

    // We only need to check the end-points for cases like 69-80 - 66 is too low, 88 is too high
    if !in_range(repeat(lower_bound, size)) {
        lower_bound += 1;
    }
    if !in_range(repeat(upper_bound, size)) {
        upper_bound -= 1;
    }

    lower_bound..=upper_bound
}

fn solve(lower: u64, upper: u64, size: usize) -> HashSet<u64> {
    calculate_bounds(lower, upper, size)
        .into_iter()
        .map(|n| repeat(n, size))
        .collect()
}
