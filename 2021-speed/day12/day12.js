const fs = require("fs");
const nodePath = require("path");
const _ = require("lodash");
//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
const input = fs
  .readFileSync(nodePath.join(__dirname, path))
  .toString()
  .trim()
  .split("\n")
  .map((x) => x.split("-"));

let paths = input
  // Add the reverse directions
  .concat(input.map(([a, b]) => [b, a]))
  // Can't re-enter start.
  .filter(([, b]) => b !== "start");

const destinations = (loc) => {
  return paths.filter(([a]) => a === loc).map(([, b]) => b);
};

const isSmallCave = (x) => x !== x.toUpperCase();

const hasRepeatSmallCave = (prevSegments) => {
  let smallCaves = prevSegments.filter(isSmallCave);
  return smallCaves.length !== Array.from(new Set(smallCaves)).length;
};

function buildPath(loc, history, isPart2) {
  if (loc === "end") return [[...history, "end"]];
  if (
    // Can't repeat small caves
    isSmallCave(loc) &&
    history.includes(loc) &&
    // ... except once, in part 2
    (!isPart2 || hasRepeatSmallCave(history))
  )
    return [];

  return destinations(loc).flatMap((dest) =>
    buildPath(dest, [...history, loc], isPart2)
  );
}

console.log(buildPath("start", [], false).length);
console.log(buildPath("start", [], true).length);
