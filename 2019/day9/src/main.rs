use std::io::{self, Read};
use intcode::{ IntcodeProgram };

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let line = buffer.lines().next().unwrap();

    let test_out = IntcodeProgram::from_str(line).with_input(&[1]).run();
    println!("Test output is {}", test_out[0]);

    let output = IntcodeProgram::from_str(line).with_input(&[2]).run();
    println!("Coordinates are {}", output[0]);



    Ok(())
}
