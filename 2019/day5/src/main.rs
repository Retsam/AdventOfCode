use intcode::{IntcodeProgram};
use std::io::{self, Read};

const PART_2: bool = true;

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let prog_str = buffer.lines().next().expect("expected a single line");

    let input = if PART_2 { 5 } else { 1 };
    let output = IntcodeProgram::from_str(prog_str)
        .with_input(&[input])
        .run();
    println!("{:?}", output[0]);

    Ok(())
}
