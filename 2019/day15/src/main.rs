use std::collections::HashMap;
use std::collections::VecDeque;
use std::io::{self, Read};
use std::cmp::{max};
use gridcode::Coord;
use intcode::IntcodeProgram;
use intcode::RunResult::{*};

#[derive(PartialEq)]
enum Tile { Empty, Wall, Unknown, Oxygen }
impl gridcode::GridCode for Tile {
    fn grid_code(&self) -> char {
        match self {
            Tile::Empty => '.',
            Tile::Wall => '#',
            Tile::Unknown => '?',
            Tile::Oxygen => 'O'
        }
    }
}

#[derive(Debug)]
struct ToExplore {
    from: Coord,
    to: Coord,
    dist: u32,
}

struct State {
    pos: Coord,
    map: gridcode::Grid<Tile>,
    explore_queue: VecDeque<ToExplore>,
    prog: IntcodeProgram,
    oxygen_vent: Option<(Coord, u32)>,
}

impl State {
    fn print(&self) {
        let overrides = vec!((self.pos, '!')).into_iter().collect();
        println!("{}", gridcode::print_with_overrides(&self.map, &overrides, &Tile::Unknown));
    }
    fn explore_from(&mut self, coord: Coord, dist: u32) {
        self.explore_queue.append(
            &mut coord.neighbors().into_iter()
                .filter(|c| !self.map.contains_key(c))
                .map(|c| ToExplore {
                    from: coord,
                    to: *c,
                    dist
                }).collect()
        );
    }
    fn move_to(&mut self, to: Coord) -> i64 {
        // println!("Move to {}", to);
        let from = self.pos;
        let input =
            if to.y < from.y { 1 }
            else if to.y > from.y { 2 }
            else if to.x < from.x { 3 }
            else { 4 };
        self.prog.add_input(&[input]);
        if let Output(out) = self.prog.run() {
            out
        } else {
            panic!("Prog stopped for input or halted unexpectedly")
        }
    }

    fn do_explore(&mut self) {
        while let Some(ToExplore { from, to, dist }) = self.explore_queue.pop_front() {
            self.print();

            self.return_to(from);
            match self.move_to(to) {
                0 => {
                    self.map.insert(to, Tile::Wall);
                }
                1 => {
                    self.map.insert(to, Tile::Empty);
                    self.pos = to;
                    self.explore_from(to, dist + 1);
                }
                2 => {
                    self.map.insert(to, Tile::Empty);
                    self.pos = to;
                    self.explore_from(to, dist + 1);
                    self.oxygen_vent = Some((to, dist));
                }
                x => panic!("Unexpected output {}", x)
            }
        }
    }
    fn return_to(&mut self, point: Coord) {
        if self.pos == point {
            return;
        }
        let path = gridcode::find_path(&self.map, self.pos, point, |c| *c == Tile::Empty)
            .expect("Couldn't find a path");
        // println!("Will return from {} to {} via {:?}", self.pos, point, path);
        for coord in path {
            match self.move_to(coord) {
                1 => { self.pos = coord; },
                2 => { self.pos = coord; },
                _ => panic!("Failed to return")
            }
        }
    }
}

fn fill_oxygen(mut map: HashMap<Coord, Tile>, vent: Coord) -> u32 {
    let mut search_queue = vec!();
    let mut max_dist = 0;
    map.insert(vent, Tile::Oxygen);
    search_queue.push((vent, 0));
    while let Some((coord, dist)) = search_queue.pop() {
        for neighbor in coord.neighbors().iter().cloned() {
            if let Some(Tile::Empty) = map.get(&neighbor) {
                map.insert(neighbor, Tile::Oxygen);
                max_dist = max(max_dist, dist + 1);
                search_queue.push((neighbor, dist + 1));
                println!("{}", gridcode::print(&map, &Tile::Unknown));
            }
        }
    }
    max_dist
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let line = buffer.lines().next().unwrap();
    let prog = IntcodeProgram::from_str(line);

    let origin = Coord::new(0, 0);

    let mut state = State {
        pos: origin,
        map: HashMap::new(),
        explore_queue: VecDeque::new(),
        prog,
        oxygen_vent: None
    };
    state.map.insert(origin, Tile::Empty);
    state.explore_from(origin, 1);

    state.do_explore();

    state.print();
    let (vent, dist) = state.oxygen_vent.expect("Failed to find vent");
    println!("Found oxygen system at {} ({} distance)", vent, dist);

    let time = fill_oxygen(state.map, vent);
    println!("Filled with oxygen after {}", time);

    Ok(())
}
