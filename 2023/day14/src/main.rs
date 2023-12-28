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
    let (state, max) = parse_input(&buf);
    println!("{}", sum(&tilt(state), max));

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

fn tilt((blocks, rocks): State) -> State {
    let mut new_rocks = rocks.clone();

    for rock in &rocks {
        if !new_rocks.contains(rock) {
            continue;
        }
        roll_up(&blocks, &mut new_rocks, rock);
    }

    (blocks, new_rocks)
}

fn roll_up(blocks: &Blocks, rocks: &mut Rocks, &(x, y): &Coord) -> usize {
    let mut target = y;
    rocks.remove(&(x, y));
    let new_y = loop {
        if target == 0 {
            break 0;
        }
        if blocks.contains(&(x, target - 1)) {
            break target;
        }
        // If we hit another rock, roll it up, then move below it
        if rocks.contains(&(x, target - 1)) {
            break roll_up(blocks, rocks, &(x, target - 1)) + 1;
        }
        target -= 1;
    };
    rocks.insert((x, new_y));
    new_y
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
