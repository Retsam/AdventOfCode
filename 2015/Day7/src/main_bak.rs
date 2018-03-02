use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;

const FILE_NAME: &'static str = "input1.txt";

type WireSignal = i16;
type WireSignalsMap<'a> = HashMap<&'a str, WireSignal>;

#[derive(Debug)]
enum WireValue<'a> {
    Just(Operand<'a>),
    BinaryOp(BinaryOp, Operand<'a>, Operand<'a>),
    Not(Operand<'a>)
}

#[derive(Debug)]
enum BinaryOp {
    AND,
    OR,
    LSHIFT,
    RSHIFT
}

#[derive(Debug)]
enum Operand<'a> {
    Wire(&'a str),
    Literal(WireSignal)
}

//Parsing

fn parse_operand(op: &str) -> Operand {
    match op.parse::<WireSignal>() {
        Ok(value) => Operand::Literal(value),
        Err(_) => Operand::Wire(&op.to_string())
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

fn parse_line(line: &str) -> (&str, WireValue) {
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
    (name, wire_value)
}

// Evaluation

fn get_signal(operand: &Operand, wire_signals: &WireSignalsMap) -> Option<WireSignal> {
    match operand {
        &Operand::Literal(signal) => Some(signal),
        &Operand::Wire(wire_name) => wire_signals.get(wire_name).map(|i| *i)
    }
}

fn try_calculate(wire: &WireValue, wire_signals: &WireSignalsMap) -> Option<WireSignal> {
    match wire {
        &WireValue::Just(operand) =>
            get_signal(&operand, wire_signals),

        &WireValue::Not(operand) =>
            get_signal(&operand, wire_signals)
                .map(|signal| !signal),

        &WireValue::BinaryOp(op, lhs, rhs) => {
            let maybe_lhs_signal = get_signal(&lhs, wire_signals);
            let maybe_rhs_signal = get_signal(&rhs, wire_signals);
            maybe_lhs_signal.and(maybe_rhs_signal).map(|rhs_signal| {
                let lhs_signal = maybe_lhs_signal.unwrap(); //Safe since it's checked above by .and
                match op {
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

    let mut wire_signals: HashMap<&str, WireSignal> = HashMap::new();
    let mut unresolved_wires: HashMap<&str, WireValue> = HashMap::new();

    for l in input.lines() {
        let line = l.unwrap();
        let (wire_name, wire_value) = parse_line(&line);
        // println!("{}: {:?}", wire_name, wire_value);

        unresolved_wires.insert(wire_name, wire_value);
    }

    let mut shouldKeepLooping = true;
    while shouldKeepLooping {
        shouldKeepLooping = false;
        for (wire_name, wire_value) in &unresolved_wires {
            match try_calculate(wire_value, &wire_signals) {
                Some(signal) => {
                    shouldKeepLooping = true;
                    wire_signals.insert(wire_name, signal);
                },
                None => ()
            }
        }
    }
}
