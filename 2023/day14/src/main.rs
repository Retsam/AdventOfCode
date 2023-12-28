use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::Hasher;
use std::ops::Range;
use std::{
    collections::HashSet,
    io::{self, Read},
};

type Coord = (usize, usize);

type Blocks = HashSet<Coord>;
type Rocks = HashSet<Coord>;

type State = (Blocks, Rocks);

fn main() -> Result<(), String> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;
    let (mut state, max) = parse_input(&buf);
    {
        let state = state.clone();
        println!("{}", sum(&tilt(state), max));
    }
    let mut prev_states = HashMap::<u64, usize>::new();
    // for _ in 0..1_000_000_000 {
    let mut count = 0;
    prev_states.insert(hash(&state.1, max), 0);
    let cycle_start = loop {
        state = cycle(state, max);
        count += 1;
        let state_hash = hash(&state.1, max);
        let prev_entry = prev_states.get(&state_hash);
        if let Some(prev) = prev_entry {
            break prev;
        }
        prev_states.insert(state_hash, count);
    };
    let cycle_length = count - cycle_start;
    let steps_to_take = (1_000_000_000 - cycle_start) % cycle_length;
    for _ in 0..steps_to_take {
        state = cycle(state, max)
    }
    println!("{}", sum(&state, max));

    Ok(())
}

#[allow(unused)]
fn debug((blocks, rocks): &State, max_y: usize) {
    print_grid(0..max_y, 0..max_y, |coord| {
        if blocks.contains(&coord) {
            '#'
        } else if rocks.contains(&coord) {
            'O'
        } else {
            '.'
        }
    });
    println!("----")
}

fn print_grid(x_range: Range<usize>, y_range: Range<usize>, render: impl Fn(Coord) -> char) {
    let lines = y_range.map(|y| x_range.clone().map(|x| render((x, y))).collect::<String>());
    println!("{}", lines.collect::<Vec<_>>().join("\n"));
}

fn hash(rocks: &Rocks, max: usize) -> u64 {
    let mut hasher = DefaultHasher::new();
    for x in 0..max {
        for y in 0..max {
            if rocks.contains(&(x, y)) {
                hasher.write_usize(x);
                hasher.write_usize(y);
            }
        }
    }
    hasher.finish()
}

fn tilt((blocks, rocks): State) -> State {
    let mut new_rocks = Rocks::new();

    for rock in &rocks {
        if new_rocks.contains(rock) {
            let mut target = rock.1 + 1;
            while new_rocks.contains(&(rock.0, target)) {
                target += 1
            }
            new_rocks.insert((rock.0, target));
        } else {
            roll_up(&blocks, &mut new_rocks, rock);
        }
    }

    (blocks, new_rocks)
}

fn roll_up(blocks: &Blocks, rocks: &mut Rocks, &(x, y): &Coord) -> usize {
    let mut target = y;
    let new_y = loop {
        if target == 0 {
            break 0;
        }
        if blocks.contains(&(x, target - 1)) || rocks.contains(&(x, target - 1)) {
            break target;
        }
        target -= 1;
    };
    rocks.insert((x, new_y));
    new_y
}

fn cycle(mut state: State, max: usize) -> State {
    for _ in 0..4 {
        state = tilt(state);
        state = rotate(state, max);
    }
    state
}

fn rotate((blocks, rocks): State, max: usize) -> State {
    (rotate_coords(blocks, max), rotate_coords(rocks, max))
}

fn rotate_coords(set: HashSet<Coord>, max: usize) -> HashSet<Coord> {
    set.into_iter().map(|(x, y)| (max - y - 1, x)).collect()
}

fn sum((_, rocks): &State, max_y: usize) -> u32 {
    rocks.iter().map(|c| (max_y - c.1) as u32).sum()
}

fn parse_input(input: &str) -> ((Blocks, Rocks), usize) {
    let max_y = input.lines().count();
    let state = input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars()
                .enumerate()
                .map(move |(x, c)| ((x, y), c))
                .collect::<Vec<_>>()
        })
        .fold(
            (Blocks::new(), Rocks::new()),
            |(mut blocks, mut rocks), (coords, tile)| {
                match tile {
                    '#' => {
                        blocks.insert(coords);
                    }
                    'O' => {
                        rocks.insert(coords);
                    }
                    '.' => {}
                    _ => panic!("Unexpected input"),
                };
                (blocks, rocks)
            },
        );
    (state, max_y)
}
