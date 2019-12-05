use std::io::{self, Read};

const PART_2 : bool = true;

fn calc_fuel(mass: i32) -> i32 {
    // Part 1
    let fuel = (mass / 3) - 2;
    if !PART_2 {
        fuel
    } else if fuel > 0 {
        fuel + calc_fuel(fuel)
    } else {
        0 // Not fuel, might be negative
    }
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let fuel = buffer.lines()
        .map(|s| s.parse::<i32>().expect("Not a number"))
        .map(|n| calc_fuel(n))
        .fold(0, |a, b| a + b);

    println!("Total fuel: {}", fuel);
    Ok(())
}
