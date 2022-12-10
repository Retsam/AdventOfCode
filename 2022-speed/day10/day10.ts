import fs from "fs";
import _ from "lodash";
//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/

const inputFile = fs.readFileSync(path).toString().trim();
const input = inputFile.split("\n");

// register value
let x = 1;
// cycle counter
let c = 0;
let part1 = 0;
let drawing = "";

const cycle = () => {
  const lit = Math.abs((c % 40) - x) < 2;
  drawing += lit ? "#" : ".";

  c++;
  // Log the 'interesting' values
  if (c % 40 === 20) part1 += c * x;
};

for (const cmd of input) {
  if (cmd.startsWith("addx")) {
    cycle();
    cycle();
    x += parseInt(cmd.slice(5));
  } else {
    cycle();
  }
}

console.log(part1);
console.log(_.chunk(drawing, 40).map((x) => x.join("")));
