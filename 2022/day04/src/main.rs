use itertools::Itertools;
use std::io::{self, Read};

type Pair = (i32, i32);
fn read_input() -> io::Result<Vec<(Pair, Pair)>> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    Ok(buffer
        .lines()
        .map(|line| {
            line.split(',')
                .map(|range| {
                    range
                        .split('-')
                        .map(|bound| bound.parse().unwrap())
                        .collect_tuple()
                        .unwrap()
                })
                .collect_tuple()
                .unwrap()
        })
        .collect())
}

fn main() -> io::Result<()> {
    let input = read_input()?;
    let mut p1 = 0;
    let mut p2 = 0;
    for ((a1, b1), (a2, b2)) in input {
        if (a1 >= a2 && b1 <= b2) || (a2 >= a1 && b2 <= b1) {
            p1 += 1;
        }
        if b1 >= a2 && a1 <= a2 || b2 >= a1 && a2 <= a1 {
            p2 += 1;
        }
    }
    println!("Part 1: {} {}", p1, p2);
    Ok(())
}
