extern crate intcode;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let line = buffer.lines().next().expect("expected a single line");

    let mut prog = line.split(",")
        .map(|s| s.parse::<intcode::Value>().expect("Not a number"))
        .collect::<Vec<intcode::Value>>();

    let output = intcode::run_prog_with_input(&mut prog, vec!(1));
    println!("{:?}", output);

    Ok(())
}
