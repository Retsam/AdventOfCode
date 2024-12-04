use std::io::{self, Read};

use itertools::Itertools;

#[derive(Debug, Clone, Copy)]
struct Coord {
    x: i64,
    y: i64,
}
impl Coord {
    fn mv(&self, dir: Dir) -> Coord {
        use Dir::*;
        Coord {
            x: self.x
                + match dir {
                    L | UL | DL => -1,
                    R | UR | DR => 1,
                    _ => 0,
                },
            y: self.y
                + match dir {
                    U | UL | UR => -1,
                    D | DL | DR => 1,
                    _ => 0,
                },
        }
    }
}
#[derive(Copy, Clone)]
enum Dir {
    L,
    R,
    U,
    D,
    UL,
    UR,
    DL,
    DR,
}
const DIRS: [Dir; 8] = const {
    use Dir::*;
    [U, R, D, L, UL, UR, DL, DR]
};
const DIAGS: [(Dir, Dir); 2] = const {
    use Dir::*;
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

    let grid_iter = (0..grid[0].len() as i64)
        .cartesian_product(0..grid.len() as i64)
        .map(|(x, y)| Coord { x, y });

    let mut p1_count = 0;
    for start in grid_iter.clone().filter(|start| get(*start) == Some('X')) {
        for d in DIRS {
            let m = start.mv(d);
            let a = m.mv(d);
            let s = a.mv(d);
            if matches!((get(m), get(a), get(s)), (Some('M'), Some('A'), Some('S'))) {
                p1_count += 1;
            };
        }
    }
    println!("Part 1: {}", p1_count);

    let p2_count = grid_iter
        .filter(|start| get(*start) == Some('X'))
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
