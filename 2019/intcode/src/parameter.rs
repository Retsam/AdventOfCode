use std::convert::TryInto;
use super::{Value, IntcodeProgram};

type Register = usize;

#[derive(Debug)]
pub(super) enum Parameter {
    Position(Register),
    Immediate(Value),
    Relative(Value),
}
use Parameter::{*};

fn as_index(offset: i64) -> usize {
    offset.try_into().expect("Invalid index")
}

impl Parameter {
    pub fn new(val: Value, mode: Option<char>) -> Parameter {
        match mode {
            Some('0') => Position(val.try_into().unwrap()),
            Some('1') => Immediate(val),
            Some('2') => Relative(val),
            None => Position(val.try_into().unwrap()),
            Some(x) => panic!("Invalid mode {}", x),
        }
    }
    pub fn get(self, prog: &IntcodeProgram) -> Value {
        match self {
            Immediate(v) => v,
            Position(r) => prog.read(r),
            Relative(v) => prog.read(as_index(prog.relative_base + v)),
        }
    }
    pub fn set(self, prog: &mut IntcodeProgram, v: Value) {
        match self {
            Position(r) => prog.write(r, v),
            Relative(r) => prog.write(as_index(r + prog.relative_base),  v),
            _ => panic!("Not a register when expected"),
        };
    }
}
