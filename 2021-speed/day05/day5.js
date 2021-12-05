const fs = require("fs");
const nodePath = require("path");

//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
const input = fs
  .readFileSync(nodePath.join(__dirname, path))
  .toString()
  .trim()
  .split("\n");

const lines = input.map((str) =>
  str.split(" -> ").map((pair) => pair.split(",").map((x) => +x))
);

const asStr = (x, y) => `${x},${y}`;

function* pointsBetween([x1, y1], [x2, y2]) {
  let [xSign, ySign] = [Math.sign(x2 - x1), Math.sign(y2 - y1)];
  let len = Math.max(Math.abs(x1 - x2), Math.abs(y1 - y2));
  for (let [x, y, i] = [x1, y1, 0]; i <= len; x += xSign, y += ySign, i++) {
    yield [x, y];
  }
}

const isDiagonal = ([x1, y1], [x2, y2]) => x1 !== x2 && y1 !== y2;

function solve(isPartOne) {
  let points = {};

  for (const [p1, p2] of lines) {
    if (isDiagonal(p1, p2) && isPartOne) continue;

    for (const [x, y] of pointsBetween(p1, p2)) {
      points[asStr(x, y)] = (points[asStr(x, y)] ?? 0) + 1;
    }
  }
  return Object.values(points).filter((x) => x > 1).length;
}

const part1 = solve(true);
const part2 = solve(false);
console.log(part1, part2);
