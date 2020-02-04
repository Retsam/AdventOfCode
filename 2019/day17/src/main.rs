use std::collections::HashMap;
use std::io::{self, Read};
use intcode::{IntcodeProgram};
use gridcode::Coord;

enum Tile { Scaffold, Empty }
impl gridcode::GridCode for Tile {
    fn grid_code(&self) -> char {
        match self {
            Tile::Scaffold => '#',
            Tile::Empty => '.',
        }
    }
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let line = buffer.lines().next().unwrap();
    let mut prog = IntcodeProgram::from_str(line);

    let out = prog.run_until_halt();

    let mut x = 0;
    let mut y = 0;
    let mut map = HashMap::new();
    for d in out {
        let c = d as u8 as char;
        if let Some(tile) = match c {
            '#' => Some(Tile::Scaffold),
            _ => None
        } {
            map.insert(Coord::new(x, y), tile);
        }

        x += 1;
        if c == '\n' {
            x = 0;
            y += 1;
        }
    };

    println!("{}", gridcode::print(&map, &Tile::Empty));
    // println!("{:?}", out);
    let intersections: i32 = map.iter()
        .map(|(coord, _)| {
            let is_intersection = coord.neighbors()
                .iter()
                .all(|coord2| map.contains_key(coord2));
            if is_intersection {
                coord.x * coord.y
            } else { 0 }
        })
        .sum();
    println!("Alignment parameters: {}", intersections);

    Ok(())
}
