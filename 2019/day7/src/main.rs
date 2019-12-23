use std::io::{self, Read};

type AmpOrder = [i32; 5];

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

fn test_order(order: AmpOrder, mut prog: intcode::Program) -> intcode::Value {
    let mut input_val: i32 = 0;
    for phase_setting in &order {
        let input = vec!(*phase_setting, input_val);
        let output = intcode::run_prog_with_input(&mut prog, input);
        input_val = output[0];
    }
    input_val
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let prog = intcode::parse_program(
        buffer.lines().next().expect("expected a single line")
    );

    let perms = permutations();
    let max = perms.into_iter().map(|perm| {
        test_order(perm, prog.clone())
    }).max();
    println!("Max thruster signal is {:?}", max.unwrap());
    Ok(())
}
