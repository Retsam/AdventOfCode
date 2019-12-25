use std::convert::TryInto;
mod instruction;

pub type Value = i32;
type Register = usize;

type Program = Vec<i32>;

pub struct IntcodeProgram {
    ptr: usize,
    prog: Program,
    input: Vec<Value>,
    pub state: RunState,
}
pub struct ProgramResults {
    pub prog: Program,
    pub output: Vec<Value>,
}

#[derive(Debug)]
enum Parameter {
    Position(Register),
    Immediate(Value)
}

#[derive(PartialEq)]
pub enum RunState {
    Running,
    Halted,
}
use RunState::{*};

impl IntcodeProgram {
    /** Creation */
    pub fn from_str(input: &str) -> IntcodeProgram {
        IntcodeProgram::from_vec(input.split(",")
            .map(|s| s.parse::<Value>().expect("Not a number"))
            .collect::<Vec<Value>>())
    }
    pub fn from_vec(prog: Program) -> IntcodeProgram {
        IntcodeProgram {
            ptr: 0,
            state: Running,
            prog,
            input: vec!(),
        }
    }

    // For use in "fluent" style builder pattern
    pub fn with_input(mut self, input: &[Value]) -> Self {
        self.add_input(input);
        self
    }
    // For other purposes
    pub fn add_input(&mut self, input: &[Value]) {
        self.input.extend_from_slice(input)
    }

    /** Running */
    pub fn run_until_halt(mut self) -> ProgramResults {
        let mut output = vec!();
        if self.state == Halted {
            ProgramResults { output, prog: self.prog }
        } else { loop {
            match self.run_until_output() {
                None => break ProgramResults { output, prog: self.prog },
                Some(out) => output.push(out),
            }
        }}
    }

    pub fn run_until_output(&mut self) -> Option<Value> {
        loop {
            let step_result = self.step();
            if step_result.is_some() || self.state == Halted {
                break step_result;
            }
        }
    }

    /** Shortcut for run_until_halt().output */
    pub fn run(self) -> Vec<Value> {
        self.run_until_halt().output
    }

    pub fn step(&mut self) -> Option<Value> {
        instruction::Instruction::read(self).exec(self)
    }

    /** Implementation */
    fn read_ptr(&mut self) -> Value {
        let val = self.prog[self.ptr];
        self.ptr += 1;
        val
    }
    fn parse_param(&mut self, mode: Option<char>) -> Parameter {
        let val = self.read_ptr();
        match mode {
            Some('1') => Parameter::Immediate(val),
            Some('0') => Parameter::Position(val.try_into().unwrap()),
            None => Parameter::Position(val.try_into().unwrap()),
            Some(x) => panic!("Invalid mode {}", x),
        }
    }
    fn get_val(&self, p: Parameter) -> Value {
        match p {
            Parameter::Immediate(v) => v,
            Parameter::Position(r) => self.prog[r]
        }
    }
    fn set_reg(&mut self, r: Parameter, v: Value) {
        match r {
            Parameter::Position(r) => self.prog[r] = v,
            _ => panic!("Not a register when expected"),
        };
    }
}



#[cfg(test)]
mod tests {
    use super::{ IntcodeProgram as IC };
    #[test]
    fn input_output() {
        let prog = "3,0,4,0,3,0,4,0,99";
        assert_eq!(
            IC::from_str(prog).with_input(&[42, 43]).run(),
            [42, 43]
        );
    }

    #[test]
    fn param_modes() {
        IC::from_vec(vec!(1002,4,3,4,33)).run();
    }

    #[test]
    fn negative_numbers() {
        IC::from_vec(vec!(1101,100,-1,4,0)).run();
    }

    #[test]
    fn test_eq() {
        // Tests if input equals 8
        let prog = "3,9,8,9,10,9,4,9,99,-1,8";
        assert_eq!(
            IC::from_str(prog).with_input(&[8]).run(),
            [1]
        );
        assert_eq!(
            IC::from_str(prog).with_input(&[7]).run(),
            [0]
        );
    }

    #[test]
    fn test_lt() {
        // Tests if input is less than 8
        let prog = "3,9,7,9,10,9,4,9,99,-1,8";
        assert_eq!(
            IC::from_str(prog).with_input(&[8]).run(),
            [0]
        );
        assert_eq!(
            IC::from_str(prog).with_input(&[7]).run(),
            [1]
        );
    }

    #[test]
    fn test_run_until_output() {
        let mut prog = IC::from_vec(vec!(104, 104, 104, 99, 99));
        assert_eq!(prog.run_until_output(), Some(104));
        assert_eq!(prog.run_until_output(), Some(99));
        assert_eq!(prog.run_until_output(), None);
    }
}
