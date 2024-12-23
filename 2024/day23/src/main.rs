use std::collections::{HashMap, HashSet};
use std::error;
use std::hash::Hash;
use std::io::{self, Read};

use itertools::Itertools;
use petgraph::Graph;

fn parse_input(str: &str) -> Vec<(String, String)> {
    str.lines()
        .map(|l| {
            l.splitn(2, "-")
                .map(|x| x.to_string())
                .collect_tuple()
                .unwrap()
        })
        .collect_vec()
}

type Node = String;

fn main() -> Result<(), Box<dyn error::Error>> {
    let (p1, p2) = (0, 0);
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;
    let input = parse_input(buf.trim());

    let all_nodes = input
        .iter()
        .flat_map(|x| [x.0.to_string(), x.1.to_string()])
        .unique()
        .collect_vec();
    let edges: HashSet<(Node, Node)> = input.iter().cloned().collect();
    let mut connections: HashMap<Node, HashSet<Node>> = HashMap::new();
    for (a, b) in edges.iter() {
        connections.entry(a.clone()).or_default().insert(b.clone());
        connections.entry(b.clone()).or_default().insert(a.clone());
    }

    let mut triples: HashSet<[Node; 3]> = HashSet::new();

    for (a, a_neighbors) in connections.iter() {
        let pairs = a_neighbors.iter().cartesian_product(a_neighbors.iter());
        for (b, c) in pairs {
            if b == c {
                continue;
            }
            if connections
                .get(b)
                .map(|b_neighbors| b_neighbors.contains(c))
                .unwrap_or(false)
            {
                let mut trip = [a.clone(), b.clone(), c.clone()];
                trip.sort();
                triples.insert(trip);
            }
        }
    }

    let p1 = triples
        .iter()
        .filter(|nodes| nodes.iter().any(|n| n.starts_with('t')))
        .count();

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}
