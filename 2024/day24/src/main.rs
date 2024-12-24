use core::panic;
use std::collections::{HashMap, HashSet};
use std::error;
use std::io::{self, Read};

use itertools::Itertools;

type Wire = String;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct Gate {
    left: Wire,
    right: Wire,
    out: Wire,
    op: Op,
}
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
enum Op {
    And,
    Or,
    XOr,
}

fn parse_input(str: &str) -> (HashMap<Wire, bool>, Vec<Gate>) {
    let (inputs, gates) = str.splitn(2, "\n\n").collect_tuple().unwrap();
    let inputs = inputs
        .lines()
        .map(|wire| {
            let (name, val) = wire.splitn(2, ": ").collect_tuple().unwrap();
            (name.to_string(), val == "1")
        })
        .collect::<HashMap<_, _>>();
    let gates = gates
        .lines()
        .map(|gate| {
            let (lhs, out) = gate.splitn(2, " -> ").collect_tuple().unwrap();
            let (left, op, right) = lhs
                .splitn(3, ' ')
                .map(|x| x.to_string())
                .collect_tuple()
                .unwrap();
            let op = match op.as_str() {
                "AND" => Op::And,
                "OR" => Op::Or,
                "XOR" => Op::XOr,
                _ => panic!("Unexpected {op}"),
            };
            Gate {
                left,
                right,
                out: out.to_string(),
                op,
            }
        })
        .collect::<Vec<_>>();
    (inputs, gates)
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let (p1, p2) = (0, 0);

    let (inputs, gates) = parse_input(buf.trim());
    let gate_set = HashSet::<Gate>::from_iter(gates.iter().cloned());

    let p1: u64 = {
        let mut signals = inputs.clone();
        run_system(&mut signals, gate_set.clone());
        signals
            .into_iter()
            .map(|(k, v)| {
                if v && k.starts_with('z') {
                    let i = k[1..].parse::<usize>().unwrap();
                    1u64 << i
                } else {
                    0
                }
            })
            .sum()
    };

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}

fn run_system(signals: &mut HashMap<String, bool>, mut unresolved_gates: HashSet<Gate>) {
    while let Some(next_gate) = unresolved_gates.iter().cloned().find_map(|gate| {
        match (signals.get(&gate.left), signals.get(&gate.right)) {
            (Some(left), Some(right)) => Some((gate, *left, *right)),
            _ => None,
        }
    }) {
        let (gate, left, right) = next_gate;
        let val = match gate.op {
            Op::And => left && right,
            Op::Or => left || right,
            Op::XOr => left != right,
        };
        signals.insert(gate.out.clone(), val);
        unresolved_gates.remove(&gate);
    }
}
