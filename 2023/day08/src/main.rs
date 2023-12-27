use std::{
    collections::HashMap,
    io::{self, Read},
    str::FromStr,
};

#[derive(Debug)]
struct Node<'a> {
    left: &'a str,
    right: &'a str,
}

#[derive(Debug)]
enum Step {
    L,
    R,
}
impl FromStr for Step {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "L" => Ok(Step::L),
            "R" => Ok(Step::R),
            otherwise => Err(format!("Invalid {}", otherwise)),
        }
    }
}

type Input<'a> = (Vec<Step>, HashMap<&'a str, Node<'a>>);

fn main() -> Result<(), String> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let input = parse_input(&buf).expect("Failed to parse");

    let part1 = walk("AAA", &input);
    let part2 = input
        .1
        .keys()
        .filter(|node| node.ends_with('A'))
        .map(|src| walk(src, &input))
        .reduce(lcm)
        .unwrap();

    println!("{part1} {part2:?}");

    Ok(())
}

fn walk(start: &str, (path, nodes): &Input) -> usize {
    let mut steps = path.iter().cycle();
    itertools::iterate(start, |pos| {
        let step = steps.next().unwrap();
        let node = nodes.get(pos).expect("Got lost");
        match step {
            Step::L => node.left,
            Step::R => node.right,
        }
    })
    .take_while(|pos| !pos.ends_with('Z'))
    .count()
}

fn lcm(a: usize, b: usize) -> usize {
    a * b / gcd(a, b)
}

fn gcd(a: usize, b: usize) -> usize {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

fn parse_input(input: &str) -> Option<Input> {
    let mut parts = input.split("\n\n");
    let path = parts
        .next()?
        .chars()
        .map(|c| c.to_string().parse::<Step>().ok())
        .collect::<Option<Vec<_>>>()?;
    let nodes = parts
        .next()?
        .lines()
        .map(parse_node)
        .collect::<HashMap<&str, Node>>();
    Some((path, nodes))
}
fn parse_node(line: &str) -> (&str, Node) {
    // AAA = (BBB, CCC)
    let src = &line[0..3];
    let left = &line[7..10];
    let right = &line[12..15];
    (src, Node { left, right })
}
