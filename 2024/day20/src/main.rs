use std::collections::{HashSet, VecDeque};
use std::error;
use std::fmt::Display;
use std::io::{self, Read};

use itertools::Itertools;
use utils::coord::Coord;
use utils::dir::{Dir, Neighbors, DIRS};

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

const MIN_SHORTCUT: usize = 50; // 50 for example

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;
    buf = buf.trim().to_string();

    let grid = Grid::parse_with(&buf, |c| c.try_into().unwrap());

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

    let mut skips = vec![];
    let mut skips2 = vec![];

    // skipping the last three steps, can't have a shortcut when you're two spaces or less from the end
    for (i, step) in path.iter().enumerate().take(path.len() - 3) {
        let mut search = step
            .neighbors()
            .into_iter()
            .filter(|n| grid.get(*n) == Some(&Tile::Wall))
            .map(|n| (n, 1))
            .collect::<VecDeque<_>>();

        let mut visited = HashSet::<Coord>::from_iter(search.iter().map(|(n, _)| *n));

        while let Some((coord, dist)) = search.pop_front() {
            if grid.get(coord).unwrap() != &Tile::Wall {
                let j = path
                    .iter()
                    .position(|&p| p == coord)
                    .unwrap_or_else(|| panic!("Found tile not on path, {coord}"));
                let shortcut = match j.overflowing_sub(i + dist) {
                    (_, true) => continue,
                    (x, false) => x,
                };
                if dist <= 2 {
                    skips.push(shortcut);
                }
                if shortcut >= MIN_SHORTCUT {
                    skips2.push(shortcut)
                }
            } else {
                if dist == 20 {
                    continue;
                }
                for next in coord
                    .neighbors()
                    .into_iter()
                    .filter(|&n| grid.bounds.in_bounds(n) && !visited.contains(&n))
                    .collect_vec()
                {
                    visited.insert(next);
                    search.push_back((next, dist + 1));
                }
            }
        }

        // for (tile, n) in walls {
        //     for skip2 in skip1.neighbors() {
        //         if skip2 == *step || !grid.bounds.in_bounds(skip2) {
        //             continue;
        //         }
        //         if grid.get(skip2) == Some(&Tile::Wall) {
        //             continue;
        //         } else {
        //             let j = path
        //                 .iter()
        //                 .position(|&p| p == skip2)
        //                 .unwrap_or_else(|| panic!("Found tile not on path, {skip2}"));
        //             if j > i + 2 {
        //                 skips.push(j - i - 2)
        //             }
        //         }
        //     }
        // }
    }

    let p1 = skips.len();
    let p2 = skips2.len();
    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}
