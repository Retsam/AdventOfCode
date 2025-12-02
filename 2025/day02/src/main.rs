use itertools::Itertools;
use std::error;
use std::io::{self, Read};

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

    let (mut p1, p2) = (0, 0);

    let double = |n: u64| {
        let s = n.to_string();
        format!("{s}{s}").parse::<u64>().unwrap()
    };
    let half = |n: u64| {
        let s = n.to_string();
        let len = s.len();
        n.to_string()
            .chars()
            .take((len / 2) as usize)
            .collect::<String>()
            .parse::<u64>()
            .unwrap()
    };

    for (lower, upper) in ranges {
        let in_range = |n: u64| (lower..=upper).contains(&n);
        let is_even = |n| n % 2 == 0;

        let lower_len = lower.ilog10() + 1;
        let upper_len = upper.ilog10() + 1;

        let val = if is_even(lower_len) {
            let first_half = half(lower);
            if in_range(double(first_half)) {
                Some(first_half)
            } else {
                Some(first_half + 1)
            }
        } else if is_even(upper_len) {
            Some(10u64.pow(upper_len / 2 - 1))
        } else {
            None
        };
        if let Some(mut val) = val {
            while in_range(double(val)) {
                p1 += double(val);
                val += 1;
            }
        }
    }

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}
