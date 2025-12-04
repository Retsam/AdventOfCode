// use colored::Colorize;
// use itertools::Itertools;
use std::error;
use std::io::{self, Read};
use utils::coord::Coord;
use utils::dir8::Neighbors;
use utils::grid::Grid;

fn find_removable(grid: &Grid<bool>) -> Vec<Coord> {
    let mut removable = vec![];
    for (&cell, coord) in grid.iter_with_coord() {
        if cell {
            let neighbor_count = coord
                .neighbors()
                .into_iter()
                .filter(|n| grid.get(*n) == Some(&true))
                .count();
            if neighbor_count < 4 {
                removable.push(coord);
            }
        }
    }
    removable
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let mut grid = Grid::<bool>::parse_with(&buf, |c| c == '@');

    let mut to_remove = find_removable(&grid);
    let p1 = to_remove.len();

    let mut p2 = 0;
    while !to_remove.is_empty() {
        for coord in &to_remove {
            grid.set(*coord, false);
            p2 += 1;
        }
        to_remove = find_removable(&grid);
    }

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}
