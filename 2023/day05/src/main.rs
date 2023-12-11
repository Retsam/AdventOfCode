use itertools::Itertools;
use std::collections::VecDeque;
use std::io::{self, Read};
use std::ops::Range;

type Mapping = Vec<(Range<usize>, Range<usize>)>;

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
        .map(|x| x.parse::<usize>().unwrap())
        .collect();

    let maps = groups
        .map(|group| {
            group
                .lines()
                .skip(1) // group name
                .map(|str| {
                    let (dest, src, len) = str
                        .splitn(3, ' ')
                        .map(|x| x.parse::<usize>().unwrap())
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

fn part1(seeds: &[usize], mappings: &[Mapping]) -> usize {
    seeds
        .iter()
        .map(|start| mappings.iter().fold(*start, translate))
        .min()
        .unwrap()
}

fn part2(ranges: Vec<Range<usize>>, mappings: Vec<Mapping>) -> usize {
    mappings
        .into_iter()
        .fold(ranges, |ranges, map| {
            ranges
                .into_iter()
                // Split a range wherever it crosses a mapping boundary
                // e.g. `0..10` with a mapping from 3..5 will become [0..3, 3..5, 5..10]
                .flat_map(|range| split_range_on_breakpoints(range, &map))
                .map(|range| {
                    let new_start = translate(range.start, &map);
                    // Since we've ensured the entire range is handled by one 'mapping',
                    // the end is will follow after the start point
                    //   FUN FACT: originally I used Range<u64> and range.count() here, and that made this program 2000x times slower, when not optimized
                    let new_end = new_start + range.len();
                    new_start..new_end
                })
                .collect_vec()
        })
        .iter()
        .map(|range| range.start)
        .min()
        .unwrap()
}

fn translate(input: usize, mapping: &Mapping) -> usize {
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

fn split_range_on_breakpoints(range: Range<usize>, mapping: &Mapping) -> Vec<Range<usize>> {
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
