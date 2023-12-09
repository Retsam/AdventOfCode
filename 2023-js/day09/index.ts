import fs from "fs";

const input = fs.readFileSync(process.stdin.fd).toString().trim();

const data = input
  .split("\n")
  .map((line) => line.split(" ").map((x) => parseInt(x)));

const solve = (partOne: boolean, line: number[]): number => {
  const deltas = line.slice(1).map((n, i) => n - line[i]);
  const val = new Set(deltas).size === 1 ? deltas[0] : solve(partOne, deltas);
  return partOne ? line.at(-1)! + val : line.at(0)! - val;
};

console.log(data.map(solve.bind(null, true)).reduce((a, b) => a + b));
console.log(data.map(solve.bind(null, false)).reduce((a, b) => a + b));
