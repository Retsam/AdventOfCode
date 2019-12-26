use std::convert::TryInto;
mod instruction;

pub type Value = i64;
type Register = usize;

type Program = Vec<Value>;

pub struct IntcodeProgram {
    ptr: usize,
    prog: Program,
    input: Vec<Value>,
    pub state: RunState,
    // Offset applied to memory addresses in relative mode
    relative_base: Value,
}
pub struct ProgramResults {
    pub prog: Program,
    pub output: Vec<Value>,
}

#[derive(Debug)]
enum Parameter {
    Position(Register),
    Immediate(Value),
    Relative(Value),
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
            relative_base: 0,
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
    fn read(&self, idx: usize) -> Value {
        if idx > self.prog.len() { return 0 };
        self.prog[idx]
    }
    fn write(&mut self, idx: usize, val: Value) {
        if idx >= self.prog.len() {
            self.prog.resize(idx + 1, 0);
        }
        self.prog[idx] = val
    }
    fn read_ptr(&mut self) -> Value {
        let val = self.read(self.ptr);
        self.ptr += 1;
        val
    }
    fn parse_param(&mut self, mode: Option<char>) -> Parameter {
        let val = self.read_ptr();
        match mode {
            Some('0') => Parameter::Position(val.try_into().unwrap()),
            Some('1') => Parameter::Immediate(val),
            Some('2') => Parameter::Relative(val),
            None => Parameter::Position(val.try_into().unwrap()),
            Some(x) => panic!("Invalid mode {}", x),
        }
    }
    fn get_val(&self, p: Parameter) -> Value {
        match p {
            Parameter::Immediate(v) => v,
            Parameter::Position(r) => self.read(r),
            Parameter::Relative(v) => self.read(as_index(self.relative_base + v)),
        }
    }
    fn set_reg(&mut self, r: Parameter, v: Value) {
        match r {
            Parameter::Position(r) => self.write(r, v),
            Parameter::Relative(r) => self.write(as_index(r + self.relative_base),  v),
            _ => panic!("Not a register when expected"),
        };
    }
}

fn as_index(offset: i64) -> usize {
    offset.try_into().expect("Invalid index")
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

    // Tests relative mode, and out-of-bounds reading and writing
    #[test]
    fn quine_test() {
        let prog = vec!(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99);
        let out = IC::from_vec(prog.clone()).run();
        assert_eq!(prog, out);
    }

    #[test]
    fn test_large_nums() {
        let prog = vec!(1102,34915192,34915192,7,4,7,99,0);
        assert_eq!(IC::from_vec(prog).run(), [1219070632396864]);
    }
}
