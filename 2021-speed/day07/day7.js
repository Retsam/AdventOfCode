const fs = require("fs");
const nodePath = require("path");
/*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
const input = fs
  .readFileSync(nodePath.join(__dirname, path))
  .toString()
  .trim()
  .split("\n")[0]
  .split(",")
  .map((x) => +x);

const min = Math.min(...input);
const max = Math.max(...input);

const range = (a, b) => Array.from(new Array(b - a)).map((_, i) => i + a);
const sum = (arr) => arr.reduce((a, b) => a + b);

const shortestBy = (fuelCost) =>
  Math.min(
    ...range(min, max).map((x) =>
      sum(input.map((y) => fuelCost(Math.abs(y - x))))
    )
  );

const part1 = shortestBy((x) => x);
const part2 = shortestBy((x) => (x * (x + 1)) / 2);
console.log(part1, part2);
