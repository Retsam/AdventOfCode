const fs = require("fs");

const path = "input.txt";
// const path = "example.txt";
const input = fs.readFileSync(path).toString().trim();

const data = input.split("\n").map((x) => parseInt(x));

// part 1
const countIncreases = (data) =>
  data
    // Compare each number to the previous in the array, and check if it increased
    .slice(1)
    .map((x, i) => (x > data[i] ? 1 : 0))
    // count the times it increased
    .reduce((a, b) => a + b);

console.log(countIncreases(data));

// Part 2
const partialSums = data.slice(2).map((x, i) => x + data[i] + data[i + 1]);

console.log(countIncreases(partialSums));
