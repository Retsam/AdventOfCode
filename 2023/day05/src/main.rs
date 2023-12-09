use itertools::Itertools;
use std::io::{self, Read};
use std::ops::Range;

type Mapping = Vec<(Range<u64>, Range<u64>)>;

fn main() -> Result<(), String> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let mut groups = buf.split("\n\n");
    let seeds: Vec<_> = groups
        .next()
        .unwrap()
        .trim_start_matches("seeds: ")
        .split(' ')
        .map(|x| x.parse::<u64>().unwrap())
        .collect();

    let maps = groups
        .map(|group| {
            group
                .lines()
                .skip(1) // group name
                .map(|str| {
                    let (dest, src, len) = str
                        .splitn(3, ' ')
                        .map(|x| x.parse::<u64>().unwrap())
                        .collect_tuple()
                        .unwrap();
                    (src..src + len, dest..dest + len)
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let part1 = part1(&seeds, &maps);
    println!("{part1}");

    Ok(())
}

fn translate(input: u64, mapping: &Mapping) -> u64 {
    mapping
        .iter()
        .find_map(|(src_range, dest_range)| {
            if src_range.contains(&input) {
                Some(dest_range.start + input - src_range.start)
            } else {
                None
            }
        })
        .unwrap_or(input)
}

fn part1(seeds: &[u64], mappings: &[Mapping]) -> u64 {
    seeds
        .iter()
        .map(|start| mappings.iter().fold(*start, translate))
        .min()
        .unwrap()
}
