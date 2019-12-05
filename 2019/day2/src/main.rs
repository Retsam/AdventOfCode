use std::io::{self, Read};
type Prog = Vec<usize>;

const PART_2: bool = true;

fn run(prog: &mut Prog) -> usize {
    let mut i = 0;
    loop {
        if step(prog, i) {
            break;
        }
        i += 4;
    }
    prog[0]
}

fn step(prog: &mut Prog, ptr: usize) -> bool {
    let a = prog[ptr];
    let b = prog[ptr + 1];
    let c = prog[ptr + 2];
    let d = prog[ptr + 3];
    match a {
        1 => { prog[d] = prog[b] + prog[c]; false }
        2 => { prog[d] = prog[b] * prog[c]; false }
        99 => true,
        x => panic!("Unexpected opcode {}", x)
    }
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let line = buffer.lines().next().expect("expected a single line");

    let mut prog = line.split(",")
        .map(|s| s.parse::<usize>().expect("Not a number"))
        .collect::<Vec<usize>>();

    if !PART_2 {
        prog[1] = 12;
        prog[2] = 2;

        println!("Value at 0 is {}", run(&mut prog));
    } else {
        'outer: for x in 0..100 {
            for y in 0..100 {
                let mut _prog = prog.clone();
                _prog[1] = x;
                _prog[2] = y;
                if run(&mut _prog) == 19690720 {
                    println!("noun {} verb {}", x, y);
                    break 'outer;
                }
            }
        }
    }

    Ok(())
}

