use itertools::Itertools;
use std::{
    collections::HashMap,
    io::{self, Read},
};

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let mut l1: Vec<usize> = vec![];
    let mut l2: Vec<usize> = vec![];

    let mut counts1: HashMap<usize, usize> = HashMap::new();
    let mut counts2: HashMap<usize, usize> = HashMap::new();
    for line in buffer.lines() {
        let (a, b) = line
            .splitn(2, "   ")
            .map(|x| x.parse::<usize>().expect("Expected num"))
            .collect_tuple()
            .expect("Parse failed");
        *counts1.entry(a).or_default() += 1;
        *counts2.entry(b).or_default() += 1;
        l1.push(a);
        l2.push(b);
    }
    l1.sort();
    l2.sort();
    let p1: usize = l1.into_iter().zip(l2).map(|(a, b)| a.abs_diff(b)).sum();
    let p2: usize = counts1
        .into_iter()
        .map(|(k, v)| k * v * *counts2.entry(k).or_default())
        .sum();
    println!("Part 1: {p1}\nPart 2: {p2}");
    Ok(())
}
