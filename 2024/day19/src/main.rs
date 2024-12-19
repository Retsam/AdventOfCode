use std::collections::HashMap;
use std::error;
use std::io::{self, Read};

use itertools::Itertools;

type Towel = Vec<Color>;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
enum Color {
    R,
    G,
    B,
    U,
    W,
}
impl Color {
    fn from_str(s: &str) -> Self {
        match s {
            "r" => Self::R,
            "g" => Self::G,
            "u" => Self::U,
            "b" => Self::B,
            "w" => Self::W,
            _ => panic!("Invalid color: {s}"),
        }
    }
}

fn parse_towel(s: &str) -> Towel {
    s.chars()
        .map(|c| Color::from_str(&c.to_string()))
        .collect_vec()
}

fn parse_input(buf: String) -> (Vec<Vec<Color>>, Vec<Vec<Color>>) {
    let (towels_str, designs_str) = buf.trim().split_once("\n\n").unwrap();
    let towels = towels_str.split(", ").map(parse_towel).collect_vec();
    let designs = designs_str.split("\n").map(parse_towel).collect_vec();
    (towels, designs)
}

struct TowelTree {
    node: bool,
    children: Option<HashMap<Color, TowelTree>>,
}

impl TowelTree {
    fn new() -> Self {
        Self {
            node: false,
            children: None,
        }
    }
    fn new_with(vec: Vec<Towel>) -> Self {
        let mut n = TowelTree::new();
        for t in vec {
            n.insert(t);
        }
        n
    }
    fn insert(&mut self, towel: Towel) {
        let mut current = self;
        for t in towel {
            current = current
                .children
                .get_or_insert(HashMap::new())
                .entry(t)
                .or_insert(TowelTree::new());
        }
        current.node = true;
    }
    fn find<'a>(&'a self, other: &'a Towel) -> impl Iterator<Item = Towel> + 'a {
        struct TreeIter<'a> {
            node: Option<&'a TowelTree>,
            to_match: Towel,
            matched: Towel,
        }
        impl<'a> Iterator for TreeIter<'a> {
            type Item = Towel;

            fn next(&mut self) -> Option<Self::Item> {
                loop {
                    let next = self.to_match.pop()?;
                    self.matched.push(next);
                    let node = self.node.take()?.children.as_ref()?.get(&next)?;
                    let has_pattern = node.node;
                    self.node = Some(node);
                    if has_pattern {
                        return Some(self.matched.clone());
                    }
                }
            }
        }

        let mut to_match = other.clone();
        to_match.reverse();
        TreeIter {
            node: Some(self),
            to_match,
            matched: Vec::new(),
        }
    }
}

#[test]
fn tree_test() {
    let tree = TowelTree::new_with(vec![
        parse_towel("r"),
        parse_towel("rwb"),
        parse_towel("bw"),
    ]);
    assert_eq!(
        tree.find(&parse_towel("rwb")).collect_vec(),
        vec![parse_towel("r"), parse_towel("rwb")]
    );
}

fn part_one(towels: &[Towel], designs: &[Towel]) -> usize {
    let tree = TowelTree::new_with(towels.to_vec());
    let mut cheat = HashMap::<Towel, bool>::new();
    designs
        .iter()
        .filter(|design| is_design_possible(design, &tree, &mut cheat))
        .count()
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let (p1, p2) = (0, 0);

    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let (towels, designs) = parse_input(buf);

    let p1 = part_one(&towels, &designs);

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}

fn is_design_possible(
    design: &Towel,
    towels: &TowelTree,
    cheatsheet: &mut HashMap<Towel, bool>,
) -> bool {
    if let Some(&cheat) = cheatsheet.get(design) {
        return cheat;
    }

    let res = towels.find(design).any(|towel| {
        // println!("  Found {towel:?}");
        let rest = &design.iter().copied().skip(towel.len()).collect_vec();
        design.len() == towel.len() || is_design_possible(rest, towels, cheatsheet)
    });

    cheatsheet.insert(design.clone(), res);
    res
}
