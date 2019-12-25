extern crate intcode;
use std::io::{self, Read};
use intcode::{Value, IntcodeProgram};

const PART_2: bool = true;

fn run(prog: Vec<Value>) -> Value {
    IntcodeProgram::from_vec(prog)
        .run_until_halt()
        .prog[0]
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let prog_str = buffer.lines().next().expect("expected a single line");
    let mut prog = prog_str.split(",")
            .map(|s| s.parse::<Value>().expect("Not a number"))
            .collect::<Vec<Value>>();

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

