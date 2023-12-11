use itertools::Itertools;
use std::collections::VecDeque;
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

    let pairs = seeds
        .into_iter()
        .tuples()
        .map(|(start, len)| start..(start + len))
        .collect_vec();

    let part2 = part2(pairs, maps);
    println!("{part1} {part2}");

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

fn translate_range(range: Range<u64>, mapping: &Mapping) -> Vec<Range<u64>> {
    // Find all the start and endpoints of the translations that break up `range`, duplicate them then chunk so we go:
    // [x1, x2] -> [x1, x1, x2, x2] -> [start, x1, x1, x2, x2, end] -> [start..x1, x1..x2, x2..end]
    let mut breakpoints = mapping
        .iter()
        .flat_map(|(src_range, _)| [src_range.start, src_range.end])
        .filter(|x| *x > range.start && *x < range.end)
        .flat_map(|x| [x, x])
        .collect::<VecDeque<_>>();
    breakpoints.push_front(range.start);
    breakpoints.push_back(range.end);
    breakpoints
        .into_iter()
        .sorted()
        .tuples()
        .map(|(x, y)| x..y)
        .collect()
}

fn part2(ranges: Vec<Range<u64>>, mappings: Vec<Mapping>) -> u64 {
    mappings
        .into_iter()
        .fold(ranges, |ranges, map| {
            ranges
                .into_iter()
                .flat_map(|range| translate_range(range, &map))
                .map(|range| {
                    let new_start = translate(range.start, &map);
                    new_start..(new_start + range.count() as u64)
                })
                .collect_vec()
        })
        .iter()
        .map(|range| range.start)
        .min()
        .unwrap()
}
