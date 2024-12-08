use std::collections::HashMap;
use std::error;
use std::io::{self, Read};

use colored::Colorize;
use utils::bounds::Bounds;
use utils::coord::Coord;

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let grid: Vec<Vec<_>> = buf.lines().map(|line| line.chars().collect()).collect();

    let bounds = Bounds::from_vec(&grid);
    let mut antennae = HashMap::<Coord, char>::new();
    let mut antinodes = HashMap::<Coord, char>::new();

    let get = |c: Coord| {
        grid.get(c.y as usize)
            .and_then(|row| row.get(c.x as usize).cloned())
    };
    for c in bounds.iter() {
        let ant = get(c).unwrap();
        if ant != '.' {
            antennae.insert(c, ant);
        }
    }

    for (c1, &n) in antennae.iter() {
        for (c2, _) in antennae.iter().filter(|(c2, n2)| c1 != *c2 && **n2 == n) {
            let del = (c1.x - c2.x, c1.y - c2.y);
            let a1 = Coord {
                x: c1.x + del.0,
                y: c1.y + del.1,
            };
            let a2 = Coord {
                x: c2.x - del.0,
                y: c2.y - del.1,
            };
            if bounds.in_bounds(a1) {
                antinodes.insert(a1, n);
            }
            if bounds.in_bounds(a2) {
                antinodes.insert(a2, n);
            }
        }
    }

    println!(
        "{}",
        bounds.debug(|c| {
            let char = antennae.get(&c).unwrap_or(&'.').to_string();
            (if antinodes.contains_key(&c) {
                char.bright_red()
            } else {
                char.white()
            })
            .to_string()
        })
    );
    let part1 = antinodes.len();
    println!("Part 1: {}\nPart 2: {}", part1, 0);
    Ok(())
}
