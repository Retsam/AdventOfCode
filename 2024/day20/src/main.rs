use std::error;
use std::fmt::Display;
use std::io::{self, Read};

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

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;
    buf = buf.trim().to_string();

    let (p1, p2) = (0, 0);

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

    for (i, step) in path.iter().enumerate().take(path.len() - 3) {
        // skipping the last three steps, can't have a shortcut when you're two spaces or less from the end
        let walls = step
            .neighbors()
            .into_iter()
            .filter(|n| grid.get(*n) == Some(&Tile::Wall));
        for skip1 in walls {
            for skip2 in skip1.neighbors() {
                if skip2 == *step || !grid.bounds.in_bounds(skip2) {
                    continue;
                }
                if grid.get(skip2) == Some(&Tile::Wall) {
                    continue;
                } else {
                    let j = path
                        .iter()
                        .position(|&p| p == skip2)
                        .unwrap_or_else(|| panic!("Found tile not on path, {skip2}"));
                    if j > i + 2 {
                        skips.push(j - i - 2)
                    }
                }
            }
        }
    }

    let p1 = skips.into_iter().filter(|&s| s >= 100).count();
    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}
