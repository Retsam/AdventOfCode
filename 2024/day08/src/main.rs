use std::collections::{HashMap, HashSet};
use std::error;
use std::io::{self, Read};

use utils::coord::Coord;
use utils::grid::Grid;

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let grid = Grid::parse(&buf);

    let mut antennae = HashMap::<Coord, char>::new();
    let mut part1_anodes = HashSet::<Coord>::new();
    let mut part2_anodes = HashSet::<Coord>::new();

    for (&ant, c) in grid.iter_with_coord() {
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
                while grid.bounds.in_bounds(coord) {
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
