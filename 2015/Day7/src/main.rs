use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::collections::HashSet;
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;

const FILE_NAME: &'static str = "input.txt";

type WireSignal = u16;
type WireSignalsMap<'a> = HashMap<&'a str, WireSignal>;

#[derive(Debug)]
enum WireValue {
    Just(Operand),
    BinaryOp(BinaryOp, Operand, Operand),
    Not(Operand)
}

#[derive(Debug)]
enum BinaryOp {
    AND,
    OR,
    LSHIFT,
    RSHIFT
}

#[derive(Debug)]
enum Operand {
    Wire(String),
    Literal(WireSignal)
}

//Parsing

fn parse_operand(op: &str) -> Operand {
    match op.parse::<WireSignal>() {
        Ok(value) => Operand::Literal(value),
        Err(_) => Operand::Wire(op.to_string())
    }
}

fn parse_operator(op: &str) -> BinaryOp {
    match op {
        "OR" => BinaryOp::OR,
        "AND" => BinaryOp::AND,
        "LSHIFT" => BinaryOp::LSHIFT,
        "RSHIFT" => BinaryOp::RSHIFT,
        _ => panic!("Unexpected operator {}", op)
    }
}

fn parse_line<'a>(line: String) -> (String, WireValue) {
    let mut words = line.split(" ");

    let wire_value = match words.next().unwrap() {
        "NOT" => {
            let rhs = parse_operand(words.next().unwrap());
            WireValue::Not(rhs)
        },
        lhs_str => {
            let lhs = parse_operand(lhs_str);
            let op = words.next().unwrap();
            match op {
                "->" => WireValue::Just(lhs),
                _ => {
                    let rhs = parse_operand(words.next().unwrap());
                    WireValue::BinaryOp(parse_operator(op), lhs, rhs)
                }
            }
        }
    };

    let name = words.last().unwrap();
    (name.to_string(), wire_value)
}

// Evaluation

fn get_signal(operand: &Operand, wire_signals: &WireSignalsMap) -> Option<WireSignal> {
    match operand {
        &Operand::Literal(signal) => Some(signal),
        &Operand::Wire(ref wire_name) => wire_signals.get(wire_name.as_str()).map(|i| *i)
    }
}

fn try_calculate(wire: &WireValue, wire_signals: &WireSignalsMap) -> Option<WireSignal> {
    match *wire {
        WireValue::Just(ref operand) =>
            get_signal(&operand, wire_signals),

        WireValue::Not(ref operand) =>
            get_signal(&operand, wire_signals)
                .map(|signal| !signal),

        WireValue::BinaryOp(ref op, ref lhs, ref rhs) => {
            let maybe_lhs_signal = get_signal(&lhs, wire_signals);
            let maybe_rhs_signal = get_signal(&rhs, wire_signals);
            maybe_lhs_signal.and(maybe_rhs_signal).map(|rhs_signal| {
                let lhs_signal = maybe_lhs_signal.unwrap(); //Safe since it's checked above by .and
                match *op {
                    BinaryOp::AND => lhs_signal & rhs_signal,
                    BinaryOp::OR => lhs_signal | rhs_signal,
                    BinaryOp::LSHIFT => lhs_signal << rhs_signal,
                    BinaryOp::RSHIFT => lhs_signal >> rhs_signal
                }
            })
        }
    }
}

fn main() {
    let f = File::open(FILE_NAME).unwrap();
    let input = BufReader::new(f);

    let lines = input.lines().map(|l| l.unwrap());

    let mut wire_names: HashSet<String> = HashSet::new();
    let mut unresolved_wires: HashMap<String, WireValue> = HashMap::new();
    let mut wire_signals: HashMap<&str, WireSignal> = HashMap::new();

    for line in lines {
        let (wire_name, wire_value) = parse_line(line.clone());
        println!("{}: {:?}", wire_name, wire_value);

        // let maybe_signal = try_calculate(&wire_value, &wire_signals);
        wire_names.insert(wire_name.to_string());
        unresolved_wires.insert(wire_name.to_string(), wire_value);

        // if maybe_signal.is_some() {
        //     println!("Calculated {} as {}", maybe_signal.unwrap(), wire_name)
        // }

    }

    let mut should_keep_looping = true;
    while should_keep_looping {
        println!("LOOP");
        should_keep_looping = false;
        for wire_name in &wire_names {
            match unresolved_wires.entry(wire_name.clone()) {
                Entry::Vacant(_) => (),
                Entry::Occupied(o) => {
                    let value_was_calculated = match try_calculate(o.get(), &wire_signals) {
                        Some(signal) => {
                            println!("Calculated {} as {}", wire_name, signal);
                            wire_signals.insert(wire_name, signal);
                            true
                        },
                        None => false
                    };
                    if value_was_calculated {
                        should_keep_looping = true;
                        o.remove();
                    }
                }
            }

        }
    }
}
