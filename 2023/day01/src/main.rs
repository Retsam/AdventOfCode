use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf)?;

    println!("Part 1: {}", solve(false, &buf));

    println!("Part 2: {}", solve(true, &buf));

    Ok(())
}

fn solve(is_part_two: bool, buf: &str) -> u32 {
    buf.lines().map(|line| parse_line(is_part_two, line)).sum()
}

const DIGITS: [&str; 9] = [
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
];

fn parse_line(is_part_two: bool, line: &str) -> u32 {
    let digits: Vec<u32> = line
        .chars()
        .enumerate()
        // flat_map skips None values
        .flat_map(|(i, c)| {
            c.to_digit(10).or_else(|| {
                if !is_part_two {
                    return None;
                }
                DIGITS
                    .iter()
                    .position(|digit| line.chars().skip(i).take(digit.len()).eq(digit.chars()))
                    // Digit value is index plus one, since there's no zero
                    .map(|index| (index + 1) as u32)
            })
        })
        .collect();
    digits.first().expect("Expected to find a digit") * 10 + digits.last().unwrap()
}
