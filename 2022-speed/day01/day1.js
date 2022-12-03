const fs = require("fs");

const path = "input.txt";
const input = fs.readFileSync(path).toString().trim().split("\n\n");

const weights = input
  .map((lines) =>
    lines
      .split("\n")
      .map((x) => parseInt(x))
      .reduce((a, b) => a + b)
  )
  .sort((a, b) => b - a);
console.log(weights[0]);
console.log(weights.slice(0, 3).reduce((a, b) => a + b));
