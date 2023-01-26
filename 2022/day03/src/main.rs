use std::io::{self, Read};

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    Ok(buffer)
}

fn parse_part1(buffer: String) -> Vec<(String, String)> {
    buffer
        .lines()
        .map(|l| {
            let half = l.len() / 2;
            (
                l.chars().take(half).collect(),
                l.chars().skip(half).collect(),
            )
        })
        .collect::<Vec<(String, String)>>()
}

fn get_val(c: char) -> i32 {
    if c.is_ascii_lowercase() {
        c as i32 - 'a' as i32 + 1
    } else {
        c as i32 - 'A' as i32 + 27
    }
}

fn main() -> io::Result<()> {
    let input = read_input()?;
    let mut p1 = 0;
    for (a, b) in parse_part1(input.clone()) {
        let shared = a.chars().find(|c| b.contains(*c)).expect("No common char");
        p1 += get_val(shared);
    }

    let mut p2 = 0;
    let mut lines = input.lines();
    while let Some(a) = lines.next() {
        let (b, c) = (lines.next().unwrap(), lines.next().unwrap());
        let shared = a
            .chars()
            .find(|ch| b.contains(*ch) && c.contains(*ch))
            .expect("No common char");
        p2 += get_val(shared);
    }

    println!("Part 1: {}\nPart 2: {}", p1, p2);

    Ok(())
}
