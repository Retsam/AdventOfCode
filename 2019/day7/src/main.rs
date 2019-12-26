use std::io::{self, Read};
use intcode::{ IntcodeProgram };

const PART_2: bool = true;
type AmpOrder = [intcode::Value; 5];

fn permutations() -> Vec<AmpOrder> {
    let mut results = Vec::new();
    for i1 in 0..5 {
        for i2 in 0..4 {
            for i3 in 0..3 {
                for i4 in 0..2 {
                    let mut options = vec!(0,1,2,3,4);
                    let a = options.remove(i1);
                    let b = options.remove(i2);
                    let c = options.remove(i3);
                    let d = options.remove(i4);
                    let e = options[0];
                    results.push([a,b,c,d,e]);
                }
            }
        }
    }
    results
}

fn test_order(order: AmpOrder, prog: &str) -> intcode::Value {
    let mut input_val: intcode::Value = 0;
    for phase_setting in &order {
        let output = IntcodeProgram::from_str(prog)
            .with_input(&[*phase_setting, input_val])
            .run();
        input_val = output[0];
    }
    input_val
}

fn test_order_advanced(order: AmpOrder, prog: &str) -> intcode::Value {
    let mut progs = order.into_iter()
        .map(|phase_setting| {
            IntcodeProgram::from_str(prog)
                .with_input(&[phase_setting + 5])
        })
        .collect::<Vec<_>>();
    let mut input_val = 0;
    for i in (0..5).cycle() {
        progs[i].add_input(&[input_val]);
        match progs[i].run_until_output() {
            None => break,
            Some(v) => { input_val = v; },
        }
    }
    input_val
}


fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let prog = buffer.lines().next().expect("expected a single line");

    let perms = permutations();
    let max = perms.into_iter().map(|perm| {
        if PART_2 {
            test_order_advanced(perm, prog)
        } else {
            test_order(perm, prog)
        }
    }).max();
    println!("Max thruster signal is {:?}", max.unwrap());
    Ok(())
}
