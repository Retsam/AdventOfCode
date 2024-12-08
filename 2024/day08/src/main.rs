use std::collections::{HashMap, HashSet};
use std::error;
use std::io::{self, Read};

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
    let mut part1_anodes = HashSet::<Coord>::new();
    let mut part2_anodes = HashSet::<Coord>::new();

    let get = |c: Coord| {
        grid.get(c.y as usize)
            .and_then(|row| row.get(c.x as usize).cloned())
    };
    for (c, ant) in bounds.iter().map(|c| (c, get(c).unwrap())) {
        if ant != '.' {
            antennae.insert(c, ant);
        }
    }

    for (i, (c1, &freq)) in antennae.iter().enumerate() {
        for (c2, _) in antennae
            .iter()
            .skip(i + 1)
            .filter(|(_, &freq2)| freq2 == freq)
        {
            let del = Coord::from((c2.x - c1.x, c2.y - c1.y));

            let mut walk = |mut coord, dx, dy| {
                let mut step = 0;
                while bounds.in_bounds(coord) {
                    if step == 1 {
                        part1_anodes.insert(coord);
                    }
                    part2_anodes.insert(coord);
                    step += 1;
                    coord.x += dx;
                    coord.y += dy;
                }
            };
            walk(*c2, del.x, del.y);
            walk(*c1, -del.x, -del.y);
        }
    }

    let (part1, part2) = (part1_anodes.len(), part2_anodes.len());
    println!("Part 1: {part1}\nPart 2: {part2}",);
    Ok(())
}
