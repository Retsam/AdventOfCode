use std::collections::HashMap;
use std::error;
use std::fmt::Display;
use std::io::{self, Read};

use itertools::Itertools;
use utils::coord::Coord;
use utils::dir::{Dir, DIRS};

type Grid = utils::grid::Grid<Tile>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
    Empty,
    Wall,
    Start,
    End,
}
impl TryFrom<char> for Tile {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '.' => Ok(Self::Empty),
            '#' => Ok(Self::Wall),
            'S' => Ok(Self::Start),
            'E' => Ok(Self::End),
            _ => Err(()),
        }
    }
}
impl Display for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Empty => write!(f, "."),
            Self::Wall => write!(f, "#"),
            Self::Start => write!(f, "S"),
            Self::End => write!(f, "E"),
        }
    }
}

fn coords_within_dist(coord: &Coord, dist: usize) -> impl Iterator<Item = Coord> + '_ {
    // Note, skipping dist 1 (and 0) as they don't make sense for this problem (reminder if this code is reused)
    (2..=dist).flat_map(move |offset| coords_at_dist(coord, offset))
}

fn coords_at_dist(&Coord { x, y }: &Coord, dist: usize) -> impl Iterator<Item = Coord> {
    (0..(dist as i64)).flat_map(move |offset| {
        let inv_offset = (dist as i64) - offset;
        [
            Coord::new(x + offset, y + inv_offset),
            Coord::new(x + inv_offset, y - offset),
            Coord::new(x - offset, y - inv_offset),
            Coord::new(x - inv_offset, y + offset),
        ]
    })
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;
    buf = buf.trim().to_string();

    let grid = Grid::parse_with(&buf, |c| c.try_into().unwrap());

    let min_shortcut = if grid.bounds.w > 20 { 100 } else { 1 };

    let is_next = |tile: Coord, dir: &Dir| grid.get(tile.mv(*dir)).unwrap() != &Tile::Wall;

    let mut tile = grid.find_coord(|&t| t == Tile::Start).unwrap();
    let mut dir = DIRS.into_iter().find(|d| is_next(tile, d)).unwrap();

    let mut path = vec![tile];

    while grid.get(tile).unwrap() != &Tile::End {
        dir = DIRS
            .into_iter()
            .filter(|d| d.rev() != dir)
            .find(|d| is_next(tile, d))
            .unwrap_or_else(|| panic!("Failed at {tile}"));
        tile = tile.mv(dir);
        path.push(tile);
    }

    let mut skips = HashMap::<usize, usize>::new();
    let mut skips2 = HashMap::<usize, usize>::new();

    // skipping the last three steps, can't have a shortcut when you're two spaces or less from the end
    for (i, from) in path.iter().enumerate().take(path.len() - 3) {
        for to in coords_within_dist(from, 20) {
            if !grid.bounds.in_bounds(to) || grid.get(to) == Some(&Tile::Wall) {
                continue;
            }

            let j = path
                .iter()
                .position(|&p| p == to)
                .unwrap_or_else(|| panic!("Found tile not on path, {to}"));

            let dist = from.manhattan_dist(&to) as usize;

            let shortcut = match j.overflowing_sub(i + dist) {
                (_, true) => continue,
                (0, false) => continue,
                (x, false) => x,
            };

            if shortcut < min_shortcut {
                continue;
            }

            if dist <= 2 {
                *skips.entry(shortcut).or_default() += 1;
            }
            // This check is purely for the example since min_shortcut is already > 50
            if shortcut >= 50 {
                *skips2.entry(shortcut).or_default() += 1;
            }
        }
    }

    #[allow(unused)]
    fn debug(skips: &HashMap<usize, usize>) {
        let mut res = skips.iter().collect_vec();
        res.sort_by(|a, b| a.0.cmp(b.0));
        for (k, v) in res {
            if *v == 1 {
                println!("There is one cheat that save {k} picoseconds");
            } else {
                println!("There are {v} cheats that save {k} picoseconds");
            }
        }
    }
    let p1: usize = skips.drain().map(|(_, v)| v).sum();
    let p2: usize = skips2.drain().map(|(_, v)| v).sum();
    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}
