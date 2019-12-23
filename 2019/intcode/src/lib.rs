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
    JmpTrue(Parameter, Parameter),
    JmpFalse(Parameter, Parameter),
    Lt(Parameter, Parameter, Parameter),
    Eq(Parameter, Parameter, Parameter),
    Halt
}
use Instruction::{*};
#[derive(PartialEq)]
pub enum RunState {
    Running,
    Halted
}

pub fn parse_program(input: &str) -> Program {
    input.split(",")
        .map(|s| s.parse::<Value>().expect("Not a number"))
        .collect::<Vec<Value>>()
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
        5 => JmpTrue(get_param(), get_param()),
        6 => JmpFalse(get_param(), get_param()),
        7 => Lt(get_param(), get_param(), get_param()),
        8 => Eq(get_param(), get_param(), get_param()),
        99 => Halt,
        x => panic!("Unknown opcode {}", x),
    }
}

pub fn exec_instruction(state: &mut ProgramState, ins: Instruction) -> RunState {
    match ins {
        Add(a, b, dest) => set_reg(state, dest, get_val(state, a) + get_val(state, b)),
        Mul(a, b, dest) => set_reg(state, dest, get_val(state, a) * get_val(state, b)),
        Input(dest) => {
            let input = state.input.remove(0);
            set_reg(state, dest, input)
        },
        Out(a) => {
            state.output.push(get_val(state, a));
            RunState::Running
        },
        JmpTrue(a, ptr) => {
            if get_val(state, a) != 0 {
                state.ptr = get_val(state, ptr).try_into().unwrap();
            }
            RunState::Running
        },
        JmpFalse(a, ptr) => {
            if get_val(state, a) == 0 {
                state.ptr = get_val(state, ptr).try_into().unwrap();
            }
            RunState::Running
        },
        Lt(a, b, dest) => set_reg(state, dest, if get_val(state, a) < get_val(state , b) { 1 } else { 0 }),
        Eq(a, b, dest) => set_reg(state, dest, if get_val(state, a) == get_val(state , b) { 1 } else { 0 }),
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

    #[test]
    fn test_eq() {
        // Tests if input equals 8
        let prog = vec!(3,9,8,9,10,9,4,9,99,-1,8);
        assert_eq!(run_prog_with_input(&mut prog.clone(), vec!(8)), [1]);
        assert_eq!(run_prog_with_input(&mut prog.clone(), vec!(7)), [0]);
    }
    #[test]
    fn test_lt() {
        // Tests if input is less than 8
        let prog = vec!(3,9,7,9,10,9,4,9,99,-1,8);
        assert_eq!(run_prog_with_input(&mut prog.clone(), vec!(8)), [0]);
        assert_eq!(run_prog_with_input(&mut prog.clone(), vec!(7)), [1]);
    }
}
