mod instruction;
mod parameter;

pub type Value = i64;

type Program = Vec<Value>;

pub struct IntcodeProgram {
    ptr: usize,
    prog: Program,
    input: Vec<Value>,
    state: ProgState,
    // Offset applied to memory addresses in relative mode
    relative_base: Value,
}

#[derive(Debug, PartialEq)]
pub enum RunResult {
    Halted,
    Output(Value),
    AwaitingInput,
}

#[derive(PartialEq, Debug)]
enum ProgState {
    Running,
    Halted,
    AwaitingInput,
}
use ProgState::{*};

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
    // Run until it outputs, halts, or requires input
    pub fn run(&mut self) -> RunResult {
        if self.state == Halted {
            RunResult::Halted
        } else { loop {
            if let Some(v) = self.step() {
                break RunResult::Output(v);
            }
            match self.state {
                Halted => {break RunResult::Halted; },
                AwaitingInput => { break RunResult::AwaitingInput; },
                Running => continue,
            }
        }}
    }
    // Runs the program, collecting all output, assuming all input is provided up front
    pub fn run_until_halt(&mut self) -> Vec<Value> {
        let mut output = vec!();
        if self.state == Halted {
            panic!("Expected program to be running");
        } else { loop {
            match self.run() {
                RunResult::Halted => break output,
                RunResult::Output(out) => output.push(out),
                RunResult::AwaitingInput => { panic!("Not enough input"); }
            }
        }}
    }

    pub fn step(&mut self) -> Option<Value> {
        instruction::Instruction::read(self).exec(self)
    }

    // Get state
    pub fn get_memory(self) -> Program {
        let IntcodeProgram { prog, .. } = self;
        prog
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
        if self.ptr >= self.prog.len() {
            panic!("Ptr went beyond memory");
        }
        let val = self.read(self.ptr);
        self.ptr += 1;
        val
    }
}

#[cfg(test)]
mod tests {
    use super::{ IntcodeProgram as IC, RunResult };
    #[test]
    fn input_output() {
        let prog = "3,0,4,0,3,0,4,0,99";
        assert_eq!(
            IC::from_str(prog).with_input(&[42, 43]).run_until_halt(),
            [42, 43]
        );
    }

    #[test]
    fn param_modes() {
        IC::from_vec(vec!(1002,4,3,4,33)).run_until_halt();
    }

    #[test]
    fn negative_numbers() {
        IC::from_vec(vec!(1101,100,-1,4,0)).run_until_halt();
    }

    #[test]
    fn test_eq() {
        // Tests if input equals 8
        let prog = "3,9,8,9,10,9,4,9,99,-1,8";
        assert_eq!(
            IC::from_str(prog).with_input(&[8]).run_until_halt(),
            [1]
        );
        assert_eq!(
            IC::from_str(prog).with_input(&[7]).run_until_halt(),
            [0]
        );
    }

    #[test]
    fn test_lt() {
        // Tests if input is less than 8
        let prog = "3,9,7,9,10,9,4,9,99,-1,8";
        assert_eq!(
            IC::from_str(prog).with_input(&[8]).run_until_halt(),
            [0]
        );
        assert_eq!(
            IC::from_str(prog).with_input(&[7]).run_until_halt(),
            [1]
        );
    }

    #[test]
    fn test_run_until_output() {
        let mut prog = IC::from_vec(vec!(104, 104, 104, 99, 99));
        assert_eq!(prog.run(), RunResult::Output(104));
        assert_eq!(prog.run(), RunResult::Output(99));
        assert_eq!(prog.run(), RunResult::Halted);
    }

    // Tests relative mode, and out-of-bounds reading and writing
    #[test]
    fn quine_test() {
        let prog = vec!(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99);
        let out = IC::from_vec(prog.clone()).run_until_halt();
        assert_eq!(prog, out);
    }

    #[test]
    fn test_large_nums() {
        let prog = vec!(1102,34915192,34915192,7,4,7,99,0);
        assert_eq!(IC::from_vec(prog).run_until_halt(), [1219070632396864]);
    }

    #[test]
    fn partial_input() {
        let mut prog = IC::from_vec(vec!(3,0,4,0,99));
        prog.run();
        assert_eq!(prog.with_input(&[42]).run_until_halt(), [42]);
    }
}
