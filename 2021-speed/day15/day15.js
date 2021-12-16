const fs = require("fs");
const nodePath = require("path");
const _ = require("lodash");
//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
let _map = fs
  .readFileSync(nodePath.join(__dirname, path))
  .toString()
  .trim()
  .split("\n")
  .map((l) => l.split("").map((x) => +x));

const mx = _map.length;
const my = _map[0].length;

const map = _.range(0, mx * 5).map((x) =>
  _.range(0, my * 5).map((y) => {
    const val = _map[y % my][x % mx] + Math.floor(x / mx) + Math.floor(y / my);
    return val > 9 ? val - 9 : val;
  })
);

const goalX = mx * 5 - 1;
const goalY = my * 5 - 1;

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
const solved = new Set();
const frontier = new Set([toStr(0, 0)]);

while (allPoints.size > 0) {
  const getCost = (pt) => bestPaths[pt] ?? MAX;
  const point = _.minBy(Array.from(frontier), getCost);
  const cost = getCost(point);
  solved.add(point);
  frontier.delete(point);
  const [x, y] = point.split(",").map((x) => +x);
  if (x === goalX && y === goalY) break;
  for (const [x2, y2] of neighbors(x, y)) {
    const neighborCost = map[x2]?.[y2];
    if (neighborCost === undefined) continue;
    const neighborPt = toStr(x2, y2);
    if (!solved.has(neighborPt)) {
      frontier.add(neighborPt);
    }
    bestPaths[neighborPt] = Math.min(getCost(neighborPt), cost + map[x2][y2]);
  }
}
console.log(bestPaths[toStr(goalX, goalY)]);
