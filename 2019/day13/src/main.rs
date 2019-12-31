use std::io::{self, Read};
use intcode::{IntcodeProgram, Value};

struct ScreenState {
    ball_x: Value,
    paddle_x: Value,
    score: Value,
}
impl ScreenState {
    fn new(output: Vec<Value>) -> ScreenState {
        let mut score = 0;
        let mut ball_x = 0;
        let mut paddle_x = 0;
        let entities = output.chunks_exact(3);
        for entity in entities {
            if let [x, y, id] = entity {
                if *x == -1 && *y == 0 {
                    score = *id;
                }
                if *id == 4 {
                    ball_x = *x;
                }
                if *id == 3 {
                    paddle_x = *x;
                }
            } else { panic!("Expected chunk size of 3"); }
        }
        ScreenState {
            score, ball_x, paddle_x
        }
    }
}

fn play_game(mut prog: IntcodeProgram ) -> Value {
    loop {
        let output = prog.run_until_halt();
        let ScreenState {
            ball_x, paddle_x, score
        } = ScreenState::new(output);

        if !prog.is_running() {
            break score;
        }
        let input = if ball_x > paddle_x {
            1
        } else if ball_x < paddle_x {
            -1
        } else { 0 };
        prog.add_input(&[input]);
    }
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let line = buffer.lines().next().unwrap();
    let mut prog = IntcodeProgram::from_str(line);
    let output = prog.run_until_halt();
    let entities = output.chunks_exact(3);

    let blocks = entities.filter(|entity| {
        if let [_, _, id] = entity {
            *id == 2
        } else { panic!("Expected chunk of 3") }
    });
    println!("Found {} blocks", blocks.count());

    // Part 2
    prog = IntcodeProgram::from_str(line);
    let mut mem = prog.get_memory();
    mem[0] = 2;
    prog = IntcodeProgram::from_vec(mem);
    let score = play_game(prog);
    println!("Score is {}", score);

    Ok(())
}
