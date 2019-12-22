use std::convert::TryInto;

pub type Program = Vec<i32>;
pub type Output = Vec<i32>;
pub struct ProgramState<'a> {
    pub ptr: usize,
    pub prog: &'a mut Program,
    pub input: Vec<i32>,
    pub output: Output,
}

type Register = usize;
pub type Value = i32;

#[derive(Debug)]
pub enum Parameter {
    Position(Register),
    Immediate(Value)
}

#[derive(Debug)]
pub enum Instruction {
    Add(Parameter, Parameter, Parameter),
    Mul(Parameter, Parameter, Parameter),
    Input(Parameter),
    Out(Parameter),
    Halt
}
use Instruction::{*};
#[derive(PartialEq)]
pub enum RunState {
    Running,
    Halted
}

fn parse_val(state: &mut ProgramState) -> Value {
    let val = state.prog[state.ptr];
    state.ptr += 1;
    val
}
fn parse_param(state: &mut ProgramState, mode: Option<char>) -> Parameter {
    let val = parse_val(state);
    match mode {
        Some('1') => Parameter::Immediate(val),
        Some('0') => Parameter::Position(val.try_into().unwrap()),
        None => Parameter::Position(val.try_into().unwrap()),
        Some(x) => panic!("Invalid mode {}", x),
    }
}
fn get_val(state: &ProgramState, p: Parameter) -> Value {
    match p {
        Parameter::Immediate(v) => v,
        Parameter::Position(r) => state.prog[r]
    }
}
fn set_reg(state: &mut ProgramState, r: Parameter, v: Value) -> RunState {
    match r {
        Parameter::Position(r) => state.prog[r] = v,
        _ => panic!("Not a register when expected"),
    };
    // For convenience of implementing instruction execution
    RunState::Running
}

pub fn run_prog(prog: &mut Program) -> Output {
    run_prog_with_input(prog, Vec::new())
}
pub fn run_prog_with_input(mut prog: &mut Program, input: Vec<Value>) -> Output {
    let state = &mut ProgramState {
        ptr: 0,
        prog: &mut prog,
        output: Vec::new(),
        input,
    };
    run(state);
    state.output.to_owned()
}
pub fn run(state: &mut ProgramState) {
    state.input.reverse();
    loop {
        if step(state) == RunState::Halted { break; }
    }
}

pub fn step(state: &mut ProgramState) -> RunState {
    let ins = read_instruction(state);
    exec_instruction(state, ins)
}

pub fn read_instruction(state: &mut ProgramState) -> Instruction {
    let instruction = parse_val(state);
    let ins_str = instruction.to_string();
    let mut chars = ins_str.chars().rev();
    let (b, a) = (chars.next().unwrap_or('0'), chars.next().unwrap_or('0'));
    let opcode = vec!(a,b).into_iter().collect::<String>().parse::<Value>().expect("!!");
    let mut get_param = || {
        parse_param(state, chars.next())
    };
    match opcode {
        1 => Add(get_param(), get_param(), get_param()),
        2 => Mul(get_param(), get_param(), get_param()),
        3 => Input(get_param()),
        4 => Out(get_param()),
        99 => Halt,
        x => panic!("Unknown opcode {}", x),
    }
}

pub fn exec_instruction(state: &mut ProgramState, ins: Instruction) -> RunState {
    match ins {
        Add(a, b, dest) => set_reg(state, dest, get_val(state, a) + get_val(state, b)),
        Mul(a, b, dest) => set_reg(state, dest, get_val(state, a) * get_val(state, b)),
        Input(dest) => {
            let input = state.input.pop().expect("No input");
            set_reg(state, dest, input)
        },
        Out(a) => {
            state.output.push(get_val(state, a));
            RunState::Running
        },
        Halt => RunState::Halted,
    }
}

#[cfg(test)]
mod tests {
    use super::{*};
    #[test]
    fn input_output() {
        let output = run_prog_with_input(&mut vec!(3,0,4,0,3,0,4,0,99), vec!(42, 43));
        assert_eq!(output, vec!(42, 43))
    }

    #[test]
    fn param_modes() {
        run_prog(&mut vec!(1002,4,3,4,33));
    }

    #[test]
    fn negative_numbers() {
        run_prog(&mut vec!(1101,100,-1,4,0));
    }
}
