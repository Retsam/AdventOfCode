use std::{
    collections::HashSet,
    io::{self, Read},
};

use utils::{bounds::Bounds, coord::Coord, dir::Dir};

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
    let grid: Vec<Vec<_>> = buffer.lines().map(|line| line.chars().collect()).collect();

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
