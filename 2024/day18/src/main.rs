use std::collections::{HashMap, HashSet, VecDeque};
use std::error;
use std::io::{self, Read};

use itertools::Itertools;
use utils::bounds::Bounds;
use utils::coord::Coord;
use utils::dir::Neighbors;

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let input: Vec<Coord> = buf
        .lines()
        .map(|l| {
            let tup: (_, _) = l
                .splitn(2, ",")
                .map(|x| x.parse::<i64>().unwrap())
                .collect_tuple()
                .unwrap();
            tup.into()
        })
        .collect_vec();

    let (count, width) = if input.len() > 100 {
        (1024, 71)
    } else {
        (12, 7)
    };
    let bounds = Bounds::new(width, width);

    let p1 = search(&bounds, &input, count).unwrap();
    let p2 = part_2(&bounds, input, count);

    println!("Part 1: {p1}\nPart 2: {},{}", p2.x, p2.y);

    Ok(())
}

fn part_2(bounds: &Bounds, input: Vec<Coord>, mut min: usize) -> Coord {
    let count = input.len();
    let mut max = count;
    while max > min + 1 {
        let next = (max + min) / 2;
        if search(bounds, &input, next).is_some() {
            min = next;
        } else {
            max = next;
        }
    }
    *input.get(min).unwrap()
}

fn search(bounds: &Bounds, input: &[Coord], count: usize) -> Option<usize> {
    let corrupted: HashSet<Coord> = input.iter().copied().take(count).collect();
    let mut dist = HashMap::<Coord, usize>::new();
    let start = Coord::new(0, 0);
    dist.insert(start, 0);
    let mut search = VecDeque::from_iter([start]);
    let goal = Coord::new(bounds.w - 1, bounds.h - 1);

    while let Some(cur) = search.pop_front() {
        let cur_dist = *dist.get(&cur).unwrap();
        for n in cur.neighbors() {
            if !bounds.in_bounds(n) || corrupted.contains(&n) || dist.contains_key(&n) {
                continue;
            }
            if n == goal {
                return Some(cur_dist + 1);
            }
            dist.insert(n, cur_dist + 1);
            search.push_back(n);
            // bounds.debug(|c| {
            //     let r = if corrupted.contains(&c) {
            //         '#'.into()
            //     } else {
            //         match dist.get(&c) {
            //             Some(x) => (x % 10).to_string(),
            //             None => ".".into(),
            //         }
            //     };
            //     if c == cur {
            //         r.red().to_string()
            //     } else {
            //         r
            //     }
            // });
        }
    }
    None
}
