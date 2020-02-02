use std::io::{self, Read};

use intcode::{ IntcodeProgram, RunResult };
use std::collections::HashMap;
use std::collections::HashSet;
use gridcode::{Coord, Dir, GridCode};

use Dir::{*};

const PART_2: bool = true;

#[derive(Debug)]
enum Color { Black, White }
use Color::{*};

impl GridCode for Color {
    fn grid_code(&self) -> char {
        match self { Black => ' ', White => '#' }
    }
}

struct PaintingState {
    grid: HashMap<Coord, Color>,
    painted: HashSet<Coord>,
    position: Coord,
    dir: Dir,
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let line = buffer.lines().next().unwrap();
    let mut prog = IntcodeProgram::from_str(line);

    let mut state = PaintingState {
        grid: HashMap::new(),
        painted: HashSet::new(),
        position: Coord::new(0, 0),
        dir: Up,
    };
    if PART_2 {
        state.grid.insert(Coord::new(0, 0), White);
    }

    loop {
        match prog.run() {
            RunResult::Halted => break,
            RunResult::AwaitingInput => {
                let input = match state.grid.entry(state.position).or_insert(Black) {
                    Black => 0,
                    White => 1,
                };
                prog.add_input(&[input]);
            },
            RunResult::Output(color_val) => {
                let color = if color_val == 0 { Black } else { White };
                state.painted.insert(state.position.clone());
                state.grid.insert(state.position.clone(), color);
                if let RunResult::Output(turn_val) = prog.run() {
                    state.dir = if turn_val == 0 {
                        state.dir.ccw()
                    } else  {
                        state.dir.cw()
                    };
                    state.position = state.position.go(state.dir);
                } else {
                    panic!("Got color without turn");
                };
            }
        }
    }

    println!("Painted {} tiles", state.painted.len());
    println!("{}", gridcode::print(&state.grid, &Black));
    Ok(())
}
