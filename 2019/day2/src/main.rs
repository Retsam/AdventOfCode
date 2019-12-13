extern crate intcode;
use std::io::{self, Read};

const PART_2: bool = true;

fn run(mut prog: intcode::Program) -> usize {
    let mut prog_state = intcode::ProgramState {ptr: 0, prog: &mut prog};
    intcode::run(&mut prog_state);
    prog_state.prog[0]
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let line = buffer.lines().next().expect("expected a single line");

    let mut prog = line.split(",")
        .map(|s| s.parse::<usize>().expect("Not a number"))
        .collect::<Vec<usize>>();

    if !PART_2 {
        prog[1] = 12;
        prog[2] = 2;

        println!("Value at 0 is {}", run(prog));
    } else {
        'outer: for x in 0..100 {
            for y in 0..100 {
                let mut _prog = prog.clone();
                _prog[1] = x;
                _prog[2] = y;
                if run(_prog) == 19690720 {
                    println!("noun {} verb {}", x, y);
                    break 'outer;
                }
            }
        }
    }

    Ok(())
}

