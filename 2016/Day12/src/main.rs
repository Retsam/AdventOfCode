use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;
use std::collections::HashMap;

const FILE_NAME: &'static str = "input.txt";

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
enum Register {
    A, B, C, D
}
use Register::*;

#[derive(Copy, Clone)]
enum Value {
    Register(Register),
    Literal(i32)
}

enum Instruction {
    Cpy(Value, Register),
    Inc(Register),
    Dec(Register),
    Jnz(Value, i32)
}
use Instruction::*;

type Registers = HashMap<Register, i32>;

fn parse_value(val: &str) -> Value {
    match val.parse::<i32>() {
        Ok(v) => Value::Literal(v),
        Err(_) => Value::Register(parse_register(val))
    }
}

fn parse_register(reg: &str) -> Register {
    match reg {
        "a" => A, "b" => B, "c" => C, "d" => D, unexpected => panic!("Unexpected:{}", unexpected)
    }
}

fn parse_line(line: &str) -> Instruction {
    let mut words = line.split_whitespace();
    let cmd = words.next().unwrap();
    println!("{}", cmd);
    match cmd {
        "cpy" => Cpy(parse_value(words.next().unwrap()), parse_register(words.next().unwrap())),
        "inc" => Inc(parse_register(words.next().unwrap())),
        "dec" => Dec(parse_register(words.next().unwrap())),
        "jnz" => Jnz(parse_value(words.next().unwrap()), words.next().unwrap().parse::<i32>().unwrap()),
        unexpected => panic!("Unexpected:{}", unexpected)
    }
}

fn main() {
    let f = File::open(FILE_NAME).unwrap();
    let lines = BufReader::new(f).lines().map(|l| l.unwrap());

    let mut registers = HashMap::new();
    registers.insert(C, 1);
    let mut instruction_ptr = 0;
    // let instructions = vec![
    //     Cpy(41, A),
    //     Inc(A),
    //     Inc(A),
    //     Dec(A),
    //     Jnz(A, 2),
    //     Dec(A)
    // ];
    let instructions: Vec<_> = lines.map(|l| parse_line(&l)).collect();

    while instruction_ptr < instructions.len() {
        let instruction = &instructions[instruction_ptr];
        match instruction {
            &Cpy(val, reg) => {
                let value = match val {
                    Value::Literal(v) => v,
                    Value::Register(r) => *registers.entry(r).or_insert(0)
                };
                registers.insert(reg, value);
                instruction_ptr += 1;
            }
            &Inc(reg) => {
                let entry = registers.entry(reg).or_insert(0);
                *entry += 1;
                instruction_ptr += 1;
            }
            &Dec(reg) => {
                let entry = registers.entry(reg).or_insert(0);
                *entry -= 1;
                instruction_ptr += 1;
            },
            &Jnz(val, offset) => {
                let value = match &val {
                    &Value::Register(reg) => *registers.entry(reg).or_insert(0),
                    &Value::Literal(v) => v
                };
                if value != 0 {
                    instruction_ptr = (instruction_ptr as i32 + offset) as usize;
                } else {
                    instruction_ptr += 1;
                }
            }
        }
        println!("{:?}", registers);
    }
}
