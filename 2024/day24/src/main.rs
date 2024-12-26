use core::panic;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::hash::Hash;
use std::io::{self, Read};
use std::{error, fs};

use graphviz_rust::dot_generator::*;
use graphviz_rust::dot_structures::*;
use graphviz_rust::printer::{DotPrinter, PrinterContext};
use itertools::Itertools;

type Wire = String;
type Inputs = HashMap<Wire, bool>;

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
impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::And => "AND",
                Op::Or => "OR",
                Op::XOr => "XOR",
            }
        )
    }
}

fn parse_input(str: &str) -> (Inputs, Vec<Gate>) {
    let (inputs, gates) = str.splitn(2, "\n\n").collect_tuple().unwrap();
    let inputs = inputs
        .lines()
        .map(|wire| {
            let (name, val) = wire.splitn(2, ": ").collect_tuple().unwrap();
            (name.to_string(), val == "1")
        })
        .collect::<Inputs>();
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

    let (inputs, gates) = parse_input(buf.trim());
    render_graph(&inputs, &gates);
    let bit_size = inputs.len() / 2;
    let gate_set = HashSet::<Gate>::from_iter(gates.iter().cloned());

    let p1: u64 = {
        let mut signals = inputs.clone();
        run_system(&mut signals, gate_set.clone()).expect("Failed part 1");
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

    // Part 2, this code doesn't find the overrides just verifies them.
    //  This was actually solved by working backwards, finding each place it breaks and then manually looking at the
    //  outputted graph

    let overrides = [
        ("vkg", "z37"),
        ("ncd", "nfj"),
        ("z20", "cqr"),
        ("z15", "qnw"),
    ];

    let gate_set_with_overrides = gate_set
        .iter()
        .map(|x| {
            let out = overrides
                .iter()
                .find_map(|&(a, b)| {
                    if x.out == a {
                        Some(b.to_string())
                    } else if x.out == b {
                        Some(a.to_string())
                    } else {
                        None
                    }
                })
                .unwrap_or(x.out.to_string());
            Gate {
                left: x.left.clone(),
                right: x.right.clone(),
                out,
                op: x.op,
            }
        })
        .collect::<HashSet<_>>();

    let mut x = 0;
    let mut y = 0;
    for _i in 0..bit_size {
        let i = bit_size - _i - 1;
        x ^= 1 << i;
        y ^= 1 << i;
        let problems = test_system(x, y, bit_size, &gate_set_with_overrides).unwrap();
        if !problems.is_empty() {
            panic!("Failed at {problems:?}");
        }
    }

    let p2 = overrides
        .iter()
        .flat_map(|(a, b)| [a, b])
        .sorted()
        .join(",");

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}

// not currently using this, but it would probably be helpful for a 'real' solution
#[allow(unused)]
fn trace_wire<'a>(wire: &'a str, gates: &'a HashSet<Gate>) -> HashSet<&'a Gate> {
    let mut traced = HashSet::new();
    trace_wire_helper(wire, gates, &mut traced);
    traced
}
fn trace_wire_helper<'a>(wire: &'a str, gates: &'a HashSet<Gate>, traced: &mut HashSet<&'a Gate>) {
    for gate in gates.iter().filter(|x| x.left == *wire || x.right == *wire) {
        if !traced.contains(gate) {
            traced.insert(gate);
            trace_wire_helper(&gate.out, gates, traced);
        }
    }
}
#[allow(unused)]
fn trace_wire_rev<'a>(wire: &'a str, gates: &'a HashSet<Gate>) -> HashSet<&'a Gate> {
    let mut traced = HashSet::new();
    trace_wire_rev_helper(wire, gates, &mut traced);
    traced
}

fn trace_wire_rev_helper<'a>(
    wire: &'a str,
    gates: &'a HashSet<Gate>,
    traced: &mut HashSet<&'a Gate>,
) {
    for gate in gates.iter().filter(|x| x.out == *wire) {
        if !traced.contains(gate) {
            traced.insert(gate);
            trace_wire_helper(&gate.left, gates, traced);
            trace_wire_helper(&gate.right, gates, traced);
        }
    }
}

fn test_system(
    x: usize,
    y: usize,
    bit_size: usize,
    gates: &HashSet<Gate>,
) -> Option<HashSet<Wire>> {
    if x > 2usize.pow(bit_size as u32) || y > 2usize.pow(bit_size as u32) {
        panic!("Numbers too large for bit size");
    }
    let mut inputs: Inputs = (0..bit_size)
        .flat_map(|i| {
            [
                (format!("x{i:02}"), (x & (1 << i)) != 0),
                (format!("y{i:02}"), (y & (1 << i)) != 0),
            ]
        })
        .collect();

    run_system(&mut inputs, gates.clone()).ok()?;
    Some(
        inputs
            .iter()
            .filter(|(k, _)| k.starts_with('z'))
            .sorted()
            .filter(|(k, v)| {
                let i = k[1..].parse::<usize>().unwrap();
                let expected = (x + y) & (1 << i) != 0;
                expected != **v
            })
            .map(|(k, _)| (k.clone()))
            .collect(),
    )
}

fn run_system(
    signals: &mut HashMap<String, bool>,
    mut unresolved_gates: HashSet<Gate>,
) -> Result<(), ()> {
    while let Some(next_gate) = unresolved_gates.iter().find_map(|gate| {
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
        unresolved_gates.remove(&gate.clone());
    }
    if !unresolved_gates.is_empty() {
        Err(())
    } else {
        Ok(())
    }
}

fn render_graph(inputs: &HashMap<String, bool>, vec: &Vec<Gate>) {
    let mut g = graph!(strict di id!("id"));
    for k in inputs.keys() {
        g.add_stmt(Stmt::Node(node!(k)));
    }
    for gate in vec {
        g.add_stmt(Stmt::Node(node!(
            gate.out,
            vec![attr!(
                "shape",
                match gate.op {
                    Op::Or => "oval",
                    Op::And => "box",
                    _ => "diamond",
                }
            )]
        )));
        g.add_stmt(Stmt::Edge(edge!(node_id!(gate.left) => node_id!(gate.out))));
        g.add_stmt(Stmt::Edge(
            edge!(node_id!(gate.right) => node_id!(gate.out)),
        ));
    }
    let graph = g.print(&mut PrinterContext::default());
    fs::write("graph.dot", graph).unwrap()
}
