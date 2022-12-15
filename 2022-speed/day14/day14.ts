import fs from "fs";
import _ from "lodash";
//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/

const inputFile = fs.readFileSync(path).toString().trim();
const lines = inputFile
  .split("\n")
  .map((line) =>
    line
      .split(" -> ")
      .map((coord) => coord.split(",").map((x) => parseInt(x)) as Pos)
  );

type Pos = [number, number];
const asStr = (pos: Pos) => pos.join(",");
let maxY = 0;

const blockSet = new Map<string, "#" | "o">();
const addBlock = (x: number, y: number) => {
  if (y > maxY) maxY = y;
  blockSet.set(asStr([x, y]), "#");
};
for (const line of lines) {
  for (const segment of _.zip(line.slice(0, line.length - 1), line.slice(1))) {
    let [x, y] = segment[0]!;
    const [dx, dy] = segment[1]!;
    addBlock(x, y);
    while (x !== dx || y !== dy) {
      x += Math.sign(dx - x);
      y += Math.sign(dy - y);
      addBlock(x, y);
    }
  }
}

let s = 0;
while (true) {
  let [x, y] = [500, 0];
  while (true) {
    if (!blockSet.has(asStr([x, y + 1]))) {
      y += 1;
    } else if (!blockSet.has(asStr([x - 1, y + 1]))) {
      x -= 1;
      y += 1;
    } else if (!blockSet.has(asStr([x + 1, y + 1]))) {
      x += 1;
      y += 1;
    } else {
      blockSet.set(asStr([x, y]), "o");
      break;
    }
    if (y > maxY) break;
  }

  if (y > maxY) break;
  s++;
}
console.log(s);

let minX = 500;
let maxX = 500;
while (true) {
  s++;
  let [x, y] = [500, 0];
  while (true) {
    if (!blockSet.has(asStr([x, y + 1]))) {
      y += 1;
    } else if (!blockSet.has(asStr([x - 1, y + 1]))) {
      x -= 1;
      y += 1;
    } else if (!blockSet.has(asStr([x + 1, y + 1]))) {
      x += 1;
      y += 1;
    } else {
      minX = Math.min(x, minX);
      maxX = Math.max(x, maxX);
      blockSet.set(asStr([x, y]), "o");
      break;
    }
    if (y === maxY + 1) {
      minX = Math.min(x, minX);
      maxX = Math.max(x, maxX);
      blockSet.set(asStr([x, y]), "o");
      break;
    }
  }

  if (x === 500 && y === 0) break;
}
console.log(
  _.range(0, maxY + 2)
    .map((y) =>
      _.range(minX, maxX)
        .map((x) => blockSet.get(asStr([x, y])) ?? ".")
        .join("")
    )
    .join("\n")
);
console.log(s);
