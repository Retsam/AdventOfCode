use std::error;
use std::io::{self, Read};

use itertools::{iterate, Itertools};

fn main() -> Result<(), Box<dyn error::Error>> {
    let (p1, p2) = (0, 0);

    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let inputs = buf
        .trim()
        .lines()
        .map(|line| line.parse::<Num>().unwrap())
        .collect_vec();

    let p1: Num = inputs
        .iter()
        .map(|&num| iterate(num, |x| next_num(*x)).nth(2000).unwrap())
        .sum();

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}

type Num = u64;

fn next_num(secret: Num) -> Num {
    step3(step2(step1(secret)))
}

fn mix(n1: Num, n2: Num) -> Num {
    n1 ^ n2
}

fn prune(n: Num) -> Num {
    n % 16777216
}

fn step1(secret: Num) -> Num {
    prune(mix(secret, secret * 64))
}

fn step2(secret: Num) -> Num {
    prune(mix(secret, secret / 32))
}

fn step3(secret: Num) -> Num {
    prune(mix(secret, secret * 2048))
}
