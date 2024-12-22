use std::collections::HashMap;
use std::error;
use std::io::{self, Read};

use itertools::{iterate, Itertools};

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let inputs = buf
        .trim()
        .lines()
        .map(|line| line.parse::<Secret>().unwrap())
        .collect_vec();

    let p1: Secret = inputs
        .iter()
        .map(|&num| iterate(num, |x| next_num(*x)).nth(2000).unwrap())
        .sum();

    let prices = inputs.into_iter().map(calc_prices).collect_vec();

    let mut scores = HashMap::<Seq, u64>::new();

    for p in &prices {
        let mut current_seq_scores = HashMap::<Seq, u64>::new();
        for (seq, price) in p
            .windows(5)
            .map(|window| {
                let diffs = window
                    .windows(2)
                    .map(|w| w[1] as Diff - w[0] as Diff)
                    .collect::<Vec<Diff>>();
                Seq::from_vec(diffs)
            })
            .zip(p.iter().skip(4).map(|p| *p as u64))
        {
            current_seq_scores.entry(seq).or_insert(price);
        }
        for (seq, score) in current_seq_scores {
            *scores.entry(seq).or_default() += score;
        }
    }
    let p2 = scores.iter().max_by_key(|(_, v)| *v).unwrap().1;

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Seq([Diff; 4]);
impl Seq {
    fn from_vec(v: Vec<Diff>) -> Self {
        Self(v.try_into().unwrap())
    }
}

fn calc_prices(init: Secret) -> Vec<Price> {
    iterate(init, |&x| next_num(x))
        .map(|secret| (secret % 10) as Price)
        .take(2001)
        .collect_vec()
}

type Price = u32;
type Diff = i8;
type Secret = u64;

fn next_num(mut secret: Secret) -> Secret {
    secret = prune(mix(secret, secret * 64));
    secret = prune(mix(secret, secret / 32));
    secret = prune(mix(secret, secret * 2048));
    secret
}

fn mix(n1: Secret, n2: Secret) -> Secret {
    n1 ^ n2
}

fn prune(n: Secret) -> Secret {
    n % 16777216
}
