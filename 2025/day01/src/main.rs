use std::error;
use std::io::{self, Read};

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let ins = buf
        .lines()
        .map(|line| {
            let dist: i32 = line[1..].parse().expect("Invalid number");
            match &line[0..1] {
                "L" => -dist,
                "R" => dist,
                _ => panic!("Invalid instruction"),
            }
        })
        .collect::<Vec<i32>>();

    let mut val: i32 = 50;

    let mut p1 = 0;
    let mut p2 = 0;

    for mut delta in ins {
        if delta.abs() >= 100 {
            p2 += delta.abs() / 100;
            // We do want remainder here - delta should still be negative if it was negative
            delta %= 100;
        }
        // If we're starting at zero, we can't end up on zero and we won't be passing it
        // Bail out early to avoid double-counting
        if val == 0 {
            val = delta.rem_euclid(100);
            continue;
        }
        val += delta;

        if !(0..=100).contains(&val) {
            p2 += 1;
        }
        // We want modulus here, not remainder, to keep val in the range [0, 100)
        val = val.rem_euclid(100);

        if val == 0 {
            p1 += 1;
            p2 += 1;
        }
    }

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}
