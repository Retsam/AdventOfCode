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
  .split("\n")
  .map((l) => l.split(",").map((ll) => ll.split("-").map((x) => parseInt(x))));

const p1 = input.filter(([[a, b], [c, d]]) => {
  return (a >= c && b <= d) || (c >= a && d <= b);
}).length;

const p2 = input.filter(([[a, b], [c, d]]) => {
  return (a <= c && b >= c) || (c <= b && d >= a);
}).length;

console.log(p1, p2);
