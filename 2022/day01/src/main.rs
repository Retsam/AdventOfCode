use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let mut calories = buffer
        .split("\n\n")
        .map(|elf| elf.lines().map(|s| s.parse::<i32>().unwrap()).sum::<i32>())
        .collect::<Vec<i32>>();
    calories.sort_by(|x, y| y.cmp(x));

    let max = calories.first().unwrap();

    println!("Part 1: {}", max);

    let top3: i32 = calories.iter().take(3).sum();
    println!("Part 2: {}", top3);

    Ok(())
}
