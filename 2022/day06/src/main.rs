use std::io::{self, Read};

fn has_duplicate(string: &str) -> bool {
    let mut chars = [false; 26];
    for c in string.chars() {
        let i = (c as usize) - ('a' as usize);
        if chars[i] {
            return true;
        };
        chars[i] = true;
    }
    false
}

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    Ok(buffer)
}

fn find_marker(str: &String, size: usize) -> usize {
    (0..str.len() - size)
        .map(|i| &str[i..i + size])
        .position(|x| !has_duplicate(x))
        .expect("No marker found")
        + size
}

fn main() -> io::Result<()> {
    let input = read_input()?;

    println!(
        "Part 1: {}\nPart 2: {}",
        find_marker(&input, 4),
        find_marker(&input, 14)
    );

    Ok(())
}
