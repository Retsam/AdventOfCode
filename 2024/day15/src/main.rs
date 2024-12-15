use std::error;
use std::io::{self, Read};

use colored::Colorize;
use itertools::Itertools;
use utils::coord::Coord;
use utils::dir::Dir;
use utils::grid::Grid;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Tile {
    Empty,
    Wall,
    Box,
    Robot,
}
impl Tile {
    fn parse(c: char) -> Option<Self> {
        match c {
            '.' => Some(Self::Empty),
            '#' => Some(Self::Wall),
            'O' => Some(Self::Box),
            '@' => Some(Self::Robot),
            _ => None,
        }
    }
}

#[allow(clippy::to_string_trait_impl)]
impl ToString for Tile {
    fn to_string(&self) -> String {
        match self {
            Self::Empty => ".".into(),
            Self::Wall => "#".into(),
            Self::Box => "O".into(),
            Self::Robot => "@".red().to_string(),
        }
    }
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let (map_s, moves_s) = buf.trim().splitn(2, "\n\n").collect_tuple().unwrap();
    let mut grid = Grid::parse_with(map_s, |c| Tile::parse(c).unwrap());
    let moves = moves_s
        .lines()
        .flat_map(|line| line.chars())
        .map(|c| match c {
            '^' => Dir::U,
            '>' => Dir::R,
            'v' => Dir::D,
            '<' => Dir::L,
            x => panic!("Unexpected {x}"),
        })
        .collect_vec();

    let mut coord = grid
        .iter_with_coord()
        .find(|(t, _)| **t == Tile::Robot)
        .unwrap()
        .1;

    for dir in moves {
        println!("{dir:?}");
        do_move(&mut grid, &mut coord, dir);
    }

    let p1 = grid
        .iter_with_coord()
        .filter(|(t, _)| **t == Tile::Box)
        .map(|(_, c)| 100 * c.y + c.x)
        .sum::<i64>();

    println!("{p1}");

    Ok(())
}

fn do_move(grid: &mut Grid<Tile>, coord: &mut Coord, dir: Dir) {
    use Tile::*;
    let next = coord.mv(dir);
    let tile = *grid.get(next).unwrap();
    if tile == Wall {
        return;
    }
    if tile == Box {
        let mut next_next = next.mv(dir);
        while *grid.get(next_next).unwrap() == Box {
            next_next = next_next.mv(dir);
        }
        if *grid.get(next_next).unwrap() != Empty {
            return;
        }
        grid.set(next_next, Box);
        // fall through
    }
    grid.set(*coord, Empty);
    *coord = next;
    grid.set(*coord, Robot);
}
