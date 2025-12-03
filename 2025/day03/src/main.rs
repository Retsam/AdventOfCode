use itertools::Itertools;
use std::error;
use std::io::{self, Read};

fn next_digit(digits: Vec<u32>, remaining: u32) -> (u32, Vec<u32>) {
    let head = &digits[0..digits.len() + 1 - remaining as usize];

    let max = *head.iter().max().unwrap();
    (
        max,
        digits
            .into_iter()
            .skip_while(|x| *x != max)
            .skip(1)
            .collect_vec(),
    )
}

fn solve(mut digits: Vec<u32>, mut n: u32) -> u64 {
    let mut answer = vec![];
    while n > 1 {
        let (max, tail) = next_digit(digits, n);
        answer.push(max);
        digits = tail;
        n -= 1;
    }
    answer.push(*digits.iter().max().unwrap());
    answer.iter().fold(0, |acc, &d| acc * 10 + d as u64)
}

mod test {
    #[test]
    fn test_example() {
        let digits = "234234234234278"
            .chars()
            .map(|c| c.to_digit(10).unwrap())
            .collect::<Vec<u32>>();
        let result = super::solve(digits, 12);
        assert_eq!(result, 434234234278);
    }
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let lines = buf
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).unwrap())
                .collect::<Vec<u32>>()
        })
        .collect_vec();

    let p1: u64 = lines.iter().map(|chars| solve(chars.clone(), 2)).sum();
    let p2: u64 = lines.iter().map(|chars| solve(chars.clone(), 12)).sum();

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}
