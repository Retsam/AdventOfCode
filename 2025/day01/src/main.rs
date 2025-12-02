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
            let dir = match &line[0..1] {
                "L" => -1,
                "R" => 1,
                _ => panic!("Invalid instruction"),
            };
            let dist: i32 = line[1..].parse().expect("Invalid number");
            dir * dist
        })
        .collect::<Vec<i32>>();

    let mut val: i32 = 50;

    let mut p1 = 0;
    let mut p2 = 0;

    for delta in ins {
        let mut dist = delta.abs();
        let step = delta.signum();

        if dist >= 100 {
            p2 += dist.abs() / 100;
            dist %= 100;
        }
        for _ in 0..dist {
            val = (val + step) % 100;
            if val == 0 {
                p2 += 1;
            }
        }
        if val == 0 {
            p1 += 1;
        }
    }

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}
