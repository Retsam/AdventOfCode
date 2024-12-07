use std::{
    collections::HashSet,
    hash::Hash,
    io::{self, Read},
};

use itertools::Itertools;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
                    L => -1,
                    R => 1,
                    _ => 0,
                },
            y: self.y
                + match dir {
                    U => -1,
                    D => 1,
                    _ => 0,
                },
        }
    }
}
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
enum Dir {
    L,
    R,
    U,
    D,
}
impl Dir {
    fn cw(&self) -> Dir {
        use Dir::*;
        match self {
            L => U,
            U => R,
            R => D,
            D => L,
        }
    }
}
struct Bounds {
    min_x: i64,
    min_y: i64,
    w: i64,
    h: i64,
}
impl Bounds {
    fn new(w: i64, h: i64) -> Self {
        Self {
            min_x: 0,
            min_y: 0,
            w,
            h,
        }
    }
    fn from_vec<T>(stuff: &[Vec<T>]) -> Self {
        let w = stuff[0].len() as i64;
        let h = stuff.len() as i64;
        Bounds::new(w, h)
    }
    fn range_x(&self) -> std::ops::Range<i64> {
        self.min_x..(self.min_x + self.w)
    }
    fn range_y(&self) -> std::ops::Range<i64> {
        self.min_y..(self.min_y + self.h)
    }

    pub fn iter(&self) -> impl Iterator<Item = Coord> {
        self.range_x()
            .cartesian_product(self.range_y())
            .map(|(x, y)| Coord { x, y })
    }
    pub fn in_bounds(&self, coord: Coord) -> bool {
        coord.x >= self.min_x
            && coord.x < self.min_x + self.w
            && coord.y >= self.min_y
            && coord.y < self.min_y + self.h
    }
}

#[derive(Debug, PartialEq)]
enum RunResult {
    Loop,
    Exit(HashSet<(Coord, Dir)>),
}
fn run<F>(start: Coord, bounds: &Bounds, get: F) -> RunResult
where
    F: Fn(Coord) -> Option<char>,
{
    let mut pos = start;
    let mut dir = Dir::U;
    let mut seen = HashSet::<(Coord, Dir)>::new();
    while bounds.in_bounds(pos) {
        let already_seen = !seen.insert((pos, dir));
        if already_seen {
            return RunResult::Loop;
        }
        let next = pos.mv(dir);
        if let Some('#') = get(next) {
            dir = dir.cw();
        } else {
            pos = next;
        }
    }

    RunResult::Exit(seen)
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let grid = buffer
        .lines()
        .map(|line| line.chars().collect_vec())
        .collect_vec();

    let bounds = Bounds::from_vec(&grid);

    let get = |c: Coord| {
        grid.get(c.y as usize)
            .and_then(|row| row.get(c.x as usize).cloned())
    };

    let start = bounds.iter().find(|c| get(*c).unwrap() == '^').unwrap();

    let path = match run(start, &bounds, get) {
        RunResult::Exit(path) => path.into_iter().map(|(c, _)| c).collect::<HashSet<_>>(),
        RunResult::Loop => panic!("Loop"),
    };
    let part_1 = path.len();
    let part_2 = path
        .into_iter()
        .filter(|&test| {
            if test == start {
                return false;
            }
            let get_with_block = |c: Coord| {
                if c == test {
                    Some('#')
                } else {
                    get(c)
                }
            };
            run(start, &bounds, get_with_block) == RunResult::Loop
        })
        .count();

    println!("Part 1: {part_1}\nPart 2: {part_2}");

    Ok(())
}
