use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::error;
use std::io::{self, Read};

use itertools::Itertools;

type Val = u64;
#[derive(Debug, PartialEq, Eq)]
struct Registers {
    a: Val,
    b: Val,
    c: Val,
}

type Op = u8;
#[derive(Debug)]
enum Ins {
    Adv,
    Bxl,
    Bst,
    Jnz,
    Bxc,
    Out,
    Bdv,
    Cdv,
}
impl TryFrom<u8> for Ins {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Adv),
            1 => Ok(Self::Bxl),
            2 => Ok(Self::Bst),
            3 => Ok(Self::Jnz),
            4 => Ok(Self::Bxc),
            5 => Ok(Self::Out),
            6 => Ok(Self::Bdv),
            7 => Ok(Self::Cdv),
            _ => Err(()),
        }
    }
}
fn parse_input(input: &str) -> (Registers, Vec<u8>) {
    let (regs_str, prog_str) = input.trim().splitn(2, "\n\n").collect_tuple().unwrap();
    let (a, b, c) = regs_str
        .splitn(3, "\n")
        .map(|line| line["Register X: ".len()..].parse::<Val>().unwrap())
        .collect_tuple()
        .unwrap();
    let prog = prog_str["Program: ".len()..]
        .split(",")
        .map(|num| num.parse::<u8>().unwrap())
        .collect_vec();
    (Registers { a, b, c }, prog)
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let (mut regs, program) = parse_input(&buf);

    let output = run(&mut regs, &program);
    let p1: String = output.into_iter().map(|x| format!("{}", x)).join(",");

    let p2 = search(&program);

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}

fn run(regs: &mut Registers, program: &[Op]) -> Vec<Val> {
    let mut ip = 0;
    let mut output: Vec<Val> = Vec::new();
    while let (Some(&ins_val), Some(&op)) = (program.get(ip), program.get(ip + 1)) {
        let ins = Ins::try_from(ins_val).unwrap();
        ip += 2;

        fn combo(op: Op, regs: &Registers) -> Val {
            match op {
                x @ 0..=3 => x as Val,
                4 => regs.a,
                5 => regs.b,
                6 => regs.c,
                7 => 69, // shouldn't use this, but shouldn't panic, either
                x => panic!("Unexpected {x}"),
            }
        }

        let combo_op = combo(op, regs);
        // println!("A: {:b}, B: {:b}, C: {:b}", regs.a, regs.b, regs.c);
        // println!("{:?} {} {}", ins, op, combo_op);

        let div_op = || regs.a >> combo_op;

        match ins {
            Ins::Adv => regs.a = div_op(),
            Ins::Bxl => regs.b ^= op as Val,
            Ins::Bst => {
                regs.b = combo_op % 8;
            }
            Ins::Jnz => {
                if regs.a != 0 {
                    ip = op as usize
                }
            }
            Ins::Bxc => regs.b ^= regs.c,
            Ins::Out => output.push(combo_op % 8),
            Ins::Bdv => regs.b = div_op(),
            Ins::Cdv => regs.c = div_op(),
        }
    }
    output
}

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
struct SearchNode {
    input_tris: Vec<u8>,
}

fn combine_tris(tris: impl Iterator<Item = u8>) -> u64 {
    tris.fold(0, |prev, next| prev << 3 | next as u64)
}

fn search(program: &[Op]) -> u64 {
    // let program_rev = program.iter().rev().collect_vec();
    let mut to_test = BinaryHeap::new();
    to_test.push(Reverse(SearchNode { input_tris: vec![] }));
    while let Some(Reverse(SearchNode { input_tris })) = to_test.pop() {
        let to_match = program
            .iter()
            .rev()
            .take(input_tris.len() + 1)
            .cloned()
            .rev()
            .collect_vec();
        println!(
            "Matching {to_match:?} with prefix {:b}",
            combine_tris(input_tris.iter().cloned())
        );

        for next_tri in 0..8 {
            let a = combine_tris(input_tris.iter().cloned().chain([next_tri]));
            // println!("Trying {:b}", a);
            let output = run(&mut Registers { a, b: 0, c: 0 }, program);
            if !output
                .into_iter()
                .map(|x| x as u8)
                .eq(to_match.iter().cloned())
            {
                continue;
            }

            if to_match == program {
                return a;
            }
            println!("Found {a:b}");
            to_test.push(Reverse(SearchNode {
                input_tris: input_tris.iter().cloned().chain([next_tri]).collect_vec(),
            }));
        }
    }
    panic!("Failed");
}

/*
Bst 4  // B = A
Bxl 5  // B ^= 5
Cdv 5  // C = A / (2^B)
Bxl 6  // B ^= 6
Adv 3  // A = A / 8
Bxc 0  // B = B ^ C
Out 5  // Output B
Jnz 0  // Loop until A == 0
*/

#[allow(unused)]
fn disassemble(program: &[Op]) -> String {
    program
        .iter()
        .chunks(2)
        .into_iter()
        .map(|x| {
            let (a, b) = x.collect_tuple().unwrap();
            let ins: Ins = (*a).try_into().unwrap();
            format!("{ins:?} {b}")
        })
        .join("\n")
}

#[cfg(test)]
mod test {
    use super::*;

    fn test_regs(prog: &[Op], mut regs: Registers, expected: Registers) {
        run(&mut regs, prog);
        assert_eq!(regs, expected);
    }
    fn test_out(prog: &[Op], mut regs: Registers, expected: Vec<Val>) {
        let out = run(&mut regs, prog);
        assert_eq!(out, expected);
    }
    fn test_out_and_regs(
        prog: &[Op],
        mut regs: Registers,
        expected: Vec<Val>,
        expected_regs: Registers,
    ) {
        let out = run(&mut regs, prog);
        assert_eq!(out, expected);
        assert_eq!(regs, expected_regs);
    }
    #[test]
    fn test1() {
        test_regs(
            &[2, 6],
            Registers { a: 0, b: 0, c: 9 },
            Registers { a: 0, b: 9, c: 9 },
        );
    }
    #[test]
    fn test2() {
        test_out(
            &[5, 0, 5, 1, 5, 4],
            Registers { a: 10, b: 0, c: 0 },
            vec![0, 1, 2],
        );
    }
    #[test]
    fn test3() {
        test_out_and_regs(
            &[0, 1, 5, 4, 3, 0],
            Registers {
                a: 2024,
                b: 0,
                c: 0,
            },
            vec![4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0],
            Registers { a: 0, b: 0, c: 0 },
        );
    }
    #[test]
    fn test4() {
        test_regs(
            &[1, 7],
            Registers { a: 0, b: 29, c: 0 },
            Registers { a: 0, b: 26, c: 0 },
        );
    }
    #[test]
    fn test5() {
        test_regs(
            &[4, 0],
            Registers {
                a: 0,
                b: 2024,
                c: 43690,
            },
            Registers {
                a: 0,
                b: 44354,
                c: 43690,
            },
        );
    }
}
