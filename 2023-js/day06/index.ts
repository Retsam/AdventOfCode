import fs from "fs";
import _ from "lodash";

const input = fs.readFileSync("input.txt").toString().trim();

const lines = input.split("\n");

type Pair = readonly [number, number];

const quad = (a: number, b: number, c: number) => {
  return [
    (-b - Math.sqrt(b * b - 4 * a * c)) / 2 / a,
    (-b + Math.sqrt(b * b - 4 * a * c)) / 2 / a,
  ];
};
const solve = ([time, distance]: Pair) => {
  // x + y = time
  // x * y > distance
  // x * (time - x) > distance
  // x2 - time * x + distance > 0

  const DELTA = 0.01; // Account for solving an inequality
  const [low, high] = quad(1, -time, distance + DELTA);
  return Math.floor(high) - Math.ceil(low) + 1;
};

const _data = lines.map((line) =>
  line
    .split(/\s+/)
    .slice(1)
    .map((x) => parseInt(x))
);

// Transpose the data since it's given in columns
const part1Data = _data[0].map((x, i) => [_data[0][i], _data[1][i]] as const);
console.log(part1Data.map(solve).reduce((a, b) => a * b));

const partTwoData = lines.map((line) =>
  parseInt(
    line
      .split("")
      .filter((x) => !isNaN(parseInt(x)))
      .join("")
  )
) as [number, number];
console.log(solve(partTwoData));
