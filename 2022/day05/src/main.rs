use std::io::{self, Read};

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    Ok(buffer)
}

type Stacks = Vec<Vec<char>>;
type Instruction = (usize, usize, usize);

fn read_digit<I>(vals: I) -> Option<usize>
where
    I: Iterator<Item = char>,
{
    vals.skip_while(|c| !c.is_ascii_digit())
        .take_while(|c| c.is_ascii_digit())
        .collect::<String>()
        .parse::<usize>()
        .ok()
}

fn parse_input(input: String) -> (Stacks, Vec<Instruction>) {
    let mut stacks: Stacks = vec![];
    let (idx, _) = input.match_indices("\n\n").next().unwrap();

    let (mut setup_lines, instructions) = {
        let (setup, instructions_str) = input.split_at(idx);
        let instructions = instructions_str
            .lines()
            .skip(2)
            .map(|x| {
                let mut chars = x.chars();
                if let (Some(qty), Some(y), Some(z)) = (
                    read_digit(&mut chars),
                    read_digit(&mut chars),
                    read_digit(&mut chars),
                ) {
                    (qty, y, z)
                } else {
                    panic!("Failed to parse {}", x);
                }
            })
            .collect::<Vec<_>>();
        (setup.lines().collect::<Vec<_>>(), instructions)
    };
    // Skip the `1 2 3` line, read from bottom to top
    setup_lines.pop();
    setup_lines.reverse();
    for line in setup_lines {
        let mut i = 0;
        let mut chars = line.chars();
        while let (Some(_), Some(c), Some(_)) = (chars.next(), chars.next(), chars.next()) {
            if stacks.len() <= i {
                stacks.push(vec![]);
            }
            if c.is_alphabetic() {
                stacks[i].push(c);
            }
            i += 1;
            chars.next(); // skip the space
        }
    }
    (stacks, instructions)
}

fn move_crates(mut stacks: Stacks, instructions: &Vec<Instruction>, is_part_one: bool) -> String {
    for (qty, src, dst) in instructions {
        if is_part_one {
            for _ in 0..*qty {
                let item = stacks[src - 1]
                    .pop()
                    .unwrap_or_else(|| panic!("{} was unexpectedly empty", src));
                stacks[dst - 1].push(item);
            }
        } else {
            let mut tmp = Vec::with_capacity(*qty);
            for _ in 0..*qty {
                let item = stacks[src - 1]
                    .pop()
                    .unwrap_or_else(|| panic!("{} was unexpectedly empty", src));
                tmp.push(item);
            }
            while let Some(item) = tmp.pop() {
                stacks[dst - 1].push(item);
            }
        }
    }
    stacks
        .iter_mut()
        .map(|s| s.pop())
        .collect::<Option<String>>()
        .expect("Expected all stacks to have a value")
}

fn main() -> io::Result<()> {
    let input = read_input()?;

    let (stacks, instructions) = parse_input(input);

    println!(
        "Part 1: {}\nPart 2: {}",
        move_crates(stacks.clone(), &instructions, true),
        move_crates(stacks, &instructions, false)
    );

    Ok(())
}
