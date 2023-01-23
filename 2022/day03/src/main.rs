use std::io::{self, Read};

fn parse_input() -> io::Result<Vec<(String, String)>> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let r = buffer
        .lines()
        .map(|l| {
            let half = l.len() / 2;
            (
                l.chars().take(half).collect(),
                l.chars().skip(half).collect(),
            )
        })
        .collect::<Vec<(String, String)>>();
    Ok(r)
}

fn get_val(c: char) -> i32 {
    if c.is_ascii_lowercase() {
        c as i32 - 'a' as i32 + 1
    } else {
        c as i32 - 'A' as i32 + 27
    }
}

fn main() -> io::Result<()> {
    let mut sum = 0;
    for (a, b) in parse_input()? {
        let shared = a.chars().find(|c| b.contains(*c)).expect("No common char");
        sum += get_val(shared);
    }

    println!("{}", sum);

    Ok(())
}
