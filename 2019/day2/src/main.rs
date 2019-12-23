extern crate intcode;
use std::io::{self, Read};
use intcode::{Value};

const PART_2: bool = true;

fn run(mut prog: intcode::Program) -> Value {
    intcode::run_prog(&mut prog);
    prog[0]
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let mut prog = intcode::parse_program(
        buffer.lines().next().expect("expected a single line")
    );

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

