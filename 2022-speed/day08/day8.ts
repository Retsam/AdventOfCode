import fs from "fs";
import osPath from "path";
import _ from "lodash";
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
  .map((str) => str.split("").map((x) => parseInt(x)));

const visibleMap: Record<number, Record<number, boolean>> = {};
const markVisible = (x: number, y: number) =>
  _.set(visibleMap, `${y}.${x}`, true);

const maxX = input[0].length;
const maxY = input.length;

function checkRow(seq: Iterable<[number, number]>) {
  let highest = -1;
  for (const [x, y] of seq) {
    if (input[y][x] > highest) {
      markVisible(x, y);
      highest = input[y][x];
    }
  }
}
for (const x of _.range(maxX)) {
  checkRow(_.range(maxY).map((y) => [x, y]));
  checkRow(_.rangeRight(maxY).map((y) => [x, y]));
}
for (const y of _.range(maxY)) {
  checkRow(_.range(maxX).map((x) => [x, y]));
  checkRow(_.rangeRight(maxX).map((x) => [x, y]));
}
console.log(Object.values(visibleMap).flatMap(Object.values).length);

function countToBlock(height: number, seq: Iterable<[number, number]>) {
  let count = 0;
  for (const [x, y] of seq) {
    count++;
    if (input[y][x] >= height) break;
  }
  return count;
}
function testTree(x: number, y: number) {
  const count = countToBlock.bind(null, input[y][x]);
  const r = count(_.range(x + 1, maxX).map((x) => [x, y]));
  const d = count(_.range(y + 1, maxY).map((y) => [x, y]));
  const l = count(_.rangeRight(x).map((x) => [x, y]));
  const u = count(_.rangeRight(y).map((y) => [x, y]));
  return r * l * d * u;
}

let maxScore = -1;
for (const x of _.range(maxX)) {
  for (const y of _.range(maxY)) {
    const score = testTree(x, y);
    maxScore = Math.max(score, maxScore);
  }
}
console.log(maxScore);
