const fs = require("fs");
const nodePath = require("path");
const _ = require("lodash");
/*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
const input = fs
  .readFileSync(nodePath.join(__dirname, path))
  .toString()
  .trim()
  .split("\n");

const mx = input.length;
const my = input[0].length;

const getHeight = ([x, y]) => {
  const val = input[x]?.[y];
  return val !== undefined ? +val : 9;
};

const getLowestNeighbor = ([x, y]) => {
  const neighbors = [
    [x - 1, y],
    [x + 1, y],
    [x, y - 1],
    [x, y + 1],
  ];
  return _.minBy(neighbors, getHeight);
};

let basins = {};
let lowPoints = [];

const toStr = ([x, y]) => `${x},${y}`;

for (let x = 0; x < mx; x++) {
  for (let y = 0; y < my; y++) {
    const pos = [x, y];
    if (getHeight(pos) < getHeight(getLowestNeighbor(pos))) {
      lowPoints.push(pos);
      basins[toStr(pos)] = 1;
    }
  }
}

// Track which points we've counted to avoid double counting
let counted = new Set();
const isBasin = (pos) => toStr(pos) in basins;

function flowFrom(pos, count = 0) {
  if (!counted.has(toStr(pos))) {
    counted.add(toStr(pos));
    count++;
  }
  const lowestNeighbor = getLowestNeighbor(pos);
  if (isBasin(lowestNeighbor)) {
    basins[toStr(lowestNeighbor)] += count;
  } else {
    flowFrom(lowestNeighbor, count);
  }
}

for (let x = 0; x < mx; x++) {
  for (let y = 0; y < my; y++) {
    const pos = [x, y];
    if (getHeight(pos) === 9 || isBasin(pos) || counted.has(toStr(pos)))
      continue;
    flowFrom(pos);
  }
}

const risk = _.sum(lowPoints.map(getHeight)) + lowPoints.length;

const basinScores = _.sortBy(_.values(basins))
  .reverse()
  .slice(0, 3)
  .reduce((a, b) => a * b);

console.log(risk, basinScores);
