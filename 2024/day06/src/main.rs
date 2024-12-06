use std::{
    collections::HashSet,
    io::{self, Read},
};

use itertools::Itertools;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Coord {
    x: i64,
    y: i64,
}
impl Coord {
    fn mv(&self, dir: Dir) -> Coord {
        use Dir::*;
        Coord {
            x: self.x
                + match dir {
                    L => -1,
                    R => 1,
                    _ => 0,
                },
            y: self.y
                + match dir {
                    U => -1,
                    D => 1,
                    _ => 0,
                },
        }
    }
}
#[derive(Copy, Clone)]
enum Dir {
    L,
    R,
    U,
    D,
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let grid = buffer
        .lines()
        .map(|line| line.chars().collect_vec())
        .collect_vec();

    let w = grid[0].len() as i64;
    let h = grid.len() as i64;

    let grid_iter = (0..grid[0].len() as i64)
        .cartesian_product(0..grid.len() as i64)
        .map(|(x, y)| Coord { x, y });

    let get = |c: Coord| {
        grid.get(c.y as usize)
            .and_then(|row| row.get(c.x as usize).cloned())
    };

    let in_bounds = |c: Coord| c.x >= 0 && c.x < w && c.y >= 0 && c.y < h;

    let mut pos = grid_iter.clone().find(|c| get(*c).unwrap() == '^').unwrap();
    let mut dir = Dir::U;
    let mut seen = HashSet::<Coord>::new();
    while in_bounds(pos) {
        seen.insert(pos);
        let next = pos.mv(dir);
        if let Some('#') = get(next) {
            dir = match dir {
                Dir::U => Dir::R,
                Dir::R => Dir::D,
                Dir::D => Dir::L,
                Dir::L => Dir::U,
            };
        } else {
            pos = next;
        }
    }

    let part_1 = seen.len();
    let part_2 = "";

    println!("Part 1: {part_1}\nPart 2: {part_2}");

    Ok(())
}
