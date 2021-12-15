const fs = require("fs");
const nodePath = require("path");
const _ = require("lodash");
/*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
let map = fs
  .readFileSync(nodePath.join(__dirname, path))
  .toString()
  .trim()
  .split("\n")
  .map((l) => l.split("").map((x) => +x));

const goalX = map.length - 1;
const goalY = map[0].length - 1;

const toStr = (x, y) => `${x},${y}`;

const neighbors = (x, y) =>
  [-1, 0, 1]
    .flatMap((dx, _, arr) => arr.map((dy) => [x + dx, y + dy]))
    .filter(([xx, yy]) => (x !== xx) !== (y !== yy));

const MAX = 9999999999;

const bestPaths = {};
const allPoints = new Set(
  _.range(0, goalX + 1).flatMap((x) =>
    _.range(0, goalY + 1).map((y) => toStr(x, y))
  )
);

bestPaths[toStr(0, 0)] = 0;

while (allPoints.size > 0) {
  const getCost = (pt) => bestPaths[pt] ?? MAX;
  const point = _.minBy(Array.from(allPoints), getCost);
  const cost = getCost(point);
  allPoints.delete(point);
  const [x, y] = point.split(",").map((x) => +x);
  neighbors(x, y).forEach(([x2, y2]) => {
    const neighborCost = map[x2]?.[y2];
    if (neighborCost === undefined) return;
    const neighborPt = toStr(x2, y2);
    bestPaths[neighborPt] = Math.min(getCost(neighborPt), cost + map[x2][y2]);
  });
}
console.log(bestPaths[toStr(goalX, goalY)]);
