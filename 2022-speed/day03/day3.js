const fs = require("fs");
const osPath = require("path");
const _ = require("lodash");

//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/

const input = fs
  .readFileSync(osPath.join(__dirname, path))
  .toString()
  .trim()
  .split("\n");

const val = (c) =>
  c === c.toUpperCase()
    ? c.charCodeAt(0) - "A".charCodeAt(0) + 27
    : c.charCodeAt(0) - "a".charCodeAt(0) + 1;

const p1 = input
  .map((line) => {
    const p1 = line.slice(0, line.length / 2).split("");
    const p2 = line.slice(line.length / 2).split("");
    return p1.find((x) => p2.includes(x));
  })
  .map(val)
  .reduce((a, b) => a + b);

const p2 = _.chunk(input, 3)
  .map(([a, b, c]) => {
    return a.split("").find((ch) => b.includes(ch) && c.includes(ch));
  })
  .map(val)
  .reduce((a, b) => a + b);

console.log(p1, p2);
