use std::collections::HashMap;
use std::collections::VecDeque;
use std::io::{self, Read};
use gridcode::Coord;
use intcode::IntcodeProgram;
use intcode::RunResult::{*};

#[derive(PartialEq)]
enum Tile { Empty, Wall, Unknown }
impl gridcode::GridCode for Tile {
    fn grid_code(&self) -> char {
        match self { Tile::Empty => '.', Tile::Wall => '#', Tile::Unknown => '?' }
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
    map: HashMap<Coord, Tile>,
    explore_queue: VecDeque<ToExplore>,
    prog: IntcodeProgram,
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
    fn do_explore(&mut self) -> u32 {
        self.print();

        let ToExplore { from, to, dist } = self.explore_queue.pop_front()
            .expect("Nowhere to explore");
        self.return_to(from);
        match self.move_to(to) {
            0 => {
                self.map.insert(to, Tile::Wall);
                self.do_explore()
            }
            1 => {
                self.map.insert(to, Tile::Empty);
                self.pos = to;
                self.explore_from(to, dist + 1);
                self.do_explore()
            }
            2 => {
                dist
            }
            x => panic!("Unexpected output {}", x)
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
                _ => panic!("Failed to return")
            }
        }
    }
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
        prog
    };
    state.map.insert(origin, Tile::Empty);
    state.explore_from(origin, 1);
    let dist = state.do_explore();

    state.print();
    println!("Found oxygen system at {} distance", dist);

    Ok(())
}
