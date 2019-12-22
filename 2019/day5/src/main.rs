extern crate intcode;
use std::io::{self, Read};

const PART_2: bool = true;

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let line = buffer.lines().next().expect("expected a single line");

    let mut prog = line.split(",")
        .map(|s| s.parse::<intcode::Value>().expect("Not a number"))
        .collect::<Vec<intcode::Value>>();

    let input = if PART_2 { 5 } else { 1 };
    let output = intcode::run_prog_with_input(&mut prog, vec!(input));
    println!("{:?}", output);

    Ok(())
}
