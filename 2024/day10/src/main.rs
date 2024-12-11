use std::collections::HashSet;
use std::error;
use std::io::{self, Read};

use utils::bounds::Bounds;
use utils::coord::Coord;
use utils::dir::Dir;

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let grid: Vec<Vec<_>> = buffer
        .lines()
        .map(|line| line.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect();

    let get = |c: &Coord| {
        grid.get(c.y as usize)
            .and_then(|row| row.get(c.x as usize).cloned())
    };

    let mut p1 = 0;
    let mut p2 = 0;
    let bounds = Bounds::from_vec(&grid);
    for start in bounds.iter().filter(|&c| get(&c) == Some(0)) {
        let mut coords = HashSet::<Coord>::new();
        p2 += walk(&grid, start, &mut coords);
        p1 += coords.len();
    }
    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}

// Returns the rating (part 2) and mutates the set of found trail ends (part 1)
fn walk(grid: &Vec<Vec<u32>>, pos: Coord, found: &mut HashSet<Coord>) -> u32 {
    let get = |c: Coord| {
        grid.get(c.y as usize)
            .and_then(|row| row.get(c.x as usize).cloned())
    };
    let val = get(pos).unwrap();
    if val == 9 {
        found.insert(pos);
        return 1;
    }

    Dir::neighbors(&pos)
        .iter()
        .map(|d| pos.mv(*d))
        .filter(|c| get(*c) == Some(val + 1))
        .map(|new_pos| walk(grid, new_pos, found))
        .sum()
}
