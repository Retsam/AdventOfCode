use std::collections::HashSet;
use std::error;
use std::io::{self, Read};

use utils::coord::Coord;
use utils::dir::Neighbors;
use utils::grid::Grid;

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let grid = Grid::parse_with(&buffer, |c| c.to_digit(10).unwrap());

    let mut p1 = 0;
    let mut p2 = 0;
    for (_, start) in grid.iter_with_coord().filter(|(&item, _)| item == 0) {
        let mut coords = HashSet::<Coord>::new();
        p2 += walk(&grid, start, &mut coords);
        p1 += coords.len();
    }
    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}

// Returns the rating (part 2) and mutates the set of found trail ends (part 1)
fn walk(grid: &Grid<u32>, pos: Coord, found: &mut HashSet<Coord>) -> u32 {
    let val = *grid.get(pos).unwrap();
    if val == 9 {
        found.insert(pos);
        return 1;
    }

    pos.neighbors()
        .iter()
        .map(|d| pos.mv(*d))
        .filter(|c| grid.get(*c).copied() == Some(val + 1))
        .map(|new_pos| walk(grid, new_pos, found))
        .sum()
}
