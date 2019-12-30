use std::convert::TryInto;
use super::{Value, IntcodeProgram, Parameter, ProgState::{*}};

#[derive(Debug)]
pub(super) enum Instruction {
    Add(Parameter, Parameter, Parameter),
    Mul(Parameter, Parameter, Parameter),
    Input(Parameter),
    Out(Parameter),
    JmpTrue(Parameter, Parameter),
    JmpFalse(Parameter, Parameter),
    Lt(Parameter, Parameter, Parameter),
    Eq(Parameter, Parameter, Parameter),
    RelBaseOffset(Parameter),
    Halt
}
use Instruction::{*};

impl Instruction {
    pub(super) fn read(state: &mut IntcodeProgram) -> Instruction {
        let instruction = state.read_ptr();
        let ins_str = instruction.to_string();
        let mut chars = ins_str.chars().rev();
        let (b, a) = (chars.next().unwrap_or('0'), chars.next().unwrap_or('0'));
        let opcode = vec!(a,b).into_iter().collect::<String>().parse::<Value>().expect("!!");
        let mut get_param = || {
            state.parse_param(chars.next())
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
            9 => RelBaseOffset(get_param()),
            99 => Halt,
            x => panic!("Unknown opcode {}", x),
        }
    }

    pub(super) fn exec(self, state: &mut IntcodeProgram) -> Option<Value> {
        let mut output = None;
        match self {
            Add(a, b, dest) => state.set_reg(dest, state.get_val(a) + state.get_val(b)),
            Mul(a, b, dest) => state.set_reg(dest, state.get_val(a) * state.get_val(b)),
            Input(dest) => {
                if state.input.len() > 0 {
                    // No longer waiting for input, if we already were
                    state.state = Running;
                    let input = state.input.remove(0);
                    state.set_reg(dest, input)
                } else {
                    if state.state == AwaitingInput {
                        panic!("Awaited input, but didn't get it");
                    }
                    state.ptr -= 2;
                    state.state = AwaitingInput;
                }
            },
            Out(a) => { output = Some(state.get_val(a)) },
            JmpTrue(a, ptr) => {
                if state.get_val(a) != 0 {
                    state.ptr = state.get_val(ptr).try_into().unwrap();
                }
            },
            JmpFalse(a, ptr) => {
                if state.get_val(a) == 0 {
                    state.ptr = state.get_val(ptr).try_into().unwrap();
                }
            },
            Lt(a, b, dest) => state.set_reg(dest, if state.get_val(a) < state.get_val(b) { 1 } else { 0 }),
            Eq(a, b, dest) => state.set_reg(dest, if state.get_val(a) == state.get_val(b) { 1 } else { 0 }),
            Halt => { state.state = Halted },
            RelBaseOffset(a) => { state.relative_base += state.get_val(a); },
        };
        output
    }
}
