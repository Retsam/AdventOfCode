import * as fs from "fs";
import * as _ from "lodash";
const text = fs.readFileSync("./input.txt").toString().trim() as string;

type Registers = Record<string, number>;

let max = 0;

function testCondition(registers: Registers, register: string, op: string, val: string) {
    const rhs = parseInt(val);
    const lhs = registers[register] || 0;
    const res: boolean = ({
        ">": lhs > rhs,
        "<": lhs < rhs,
        "==": lhs === rhs,
        ">=": lhs >= rhs,
        "<=": lhs <= rhs,
        "!=": lhs !== rhs
    } as any)[op];
    console.log(`${lhs} ${op} ${rhs} === ${res}`);
    return res;
}

function runLine(registers: Registers, line: string) {
    const parts = line.split(/\s+/);
    const value = parseInt(parts[2]);
    const currentVal = registers[parts[0]] || 0;
    const newVal = parts[1] === "inc" ?
        currentVal + value :
        currentVal - value;
    if(parts.length === 3 ||
        testCondition(registers, parts[4], parts[5], parts[6])) {
            registers[parts[0]] = newVal;
            if(newVal > max) max = newVal;
        }
}

const registers = {};
const lines = text.split("\n");
console.log(lines);
lines.forEach(line => runLine(registers, line));
console.log(registers);

const maxValue = _.max(_.values(registers));
console.log("Max value: ", maxValue);
const maxRegister = _.findKey(registers, (v) => v === maxValue);
console.log("The Max register is " + maxRegister);
console.log("The all time max is " + max);
