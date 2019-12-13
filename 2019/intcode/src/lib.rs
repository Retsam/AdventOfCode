pub type Program = Vec<usize>;
pub struct ProgramState<'a> {
    pub ptr: usize,
    pub prog: &'a mut Program,
}

type Register = usize;
pub enum Instruction {
    Add(Register, Register, Register),
    Mul(Register, Register, Register),
    Halt
}
use Instruction::{*};
#[derive(PartialEq)]
pub enum RunState {
    Running,
    Halted
}

fn read_ptr(state: &mut ProgramState) -> usize {
    let val = state.prog[state.ptr];
    state.ptr += 1;
    val
}
fn get_reg(state: &ProgramState, r: Register) -> usize {
    state.prog[r]
}
fn set_reg(state: &mut ProgramState, r: Register, v: usize) -> RunState {
    state.prog[r] = v;
    // For convenience of implementing instruction execution
    RunState::Running
}

pub fn run_prog(mut prog: &mut Program) {
    run(&mut ProgramState { ptr: 0, prog: &mut prog})
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
    match read_ptr(state) {
        1 => Add(read_ptr(state), read_ptr(state), read_ptr(state)),
        2 => Mul(read_ptr(state), read_ptr(state), read_ptr(state)),
        _ => Halt,
    }
}

pub fn exec_instruction(state: &mut ProgramState, ins: Instruction) -> RunState {
    match ins {
        Add(a, b, dest) => set_reg(state, dest, get_reg(state, a) + get_reg(state, b)),
        Mul(a, b, dest) => set_reg(state, dest, get_reg(state, a) * get_reg(state, b)),
        Halt => RunState::Halted,
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
