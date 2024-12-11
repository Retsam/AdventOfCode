use itertools::Itertools;
use std::io::{self, Read};

use utils::{
    bounds::Bounds,
    coord::Coord,
    dir8::{Dir8, DIRS8},
};

const DIAGS: [(Dir8, Dir8); 2] = const {
    use Dir8::*;
    [(UR, DL), (UL, DR)]
};

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let grid = buffer
        .lines()
        .map(|line| line.chars().collect_vec())
        .collect_vec();

    let get = |c: Coord| {
        grid.get(c.y as usize)
            .and_then(|row| row.get(c.x as usize).cloned())
    };

    let bounds = Bounds::from_vec(&grid);

    let mut p1_count = 0;
    for start in bounds.iter().filter(|start| get(*start) == Some('X')) {
        for d in DIRS8 {
            let m = start.mv(d);
            let a = m.mv(d);
            let s = a.mv(d);
            if matches!((get(m), get(a), get(s)), (Some('M'), Some('A'), Some('S'))) {
                p1_count += 1;
            };
        }
    }
    println!("Part 1: {}", p1_count);

    let p2_count = bounds
        .iter()
        .filter(|start| get(*start) == Some('A'))
        .filter(|start| {
            DIAGS.into_iter().all(|(a, b)| {
                get(start.mv(a)) == Some('M') && get(start.mv(b)) == Some('S')
                    || get(start.mv(a)) == Some('S') && get(start.mv(b)) == Some('M')
            })
        })
        .count();

    println!("Part 2: {}", p2_count);

    Ok(())
}
