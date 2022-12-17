import fs from "fs";
import _ from "lodash";
//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/

const inputFile = fs.readFileSync(path).toString().trim();
const moves = inputFile.split("");

const rocks = fs
  .readFileSync("rocks.txt")
  .toString()
  .trim()
  .split("\n\n")
  .map((rockPattern) => {
    return rockPattern
      .split("\n")
      .flatMap((line, y, { length }) =>
        line
          .split("")
          .flatMap((char, x) =>
            char === "#" ? [[x, length - y - 1] as const] : []
          )
      );
  });

type Pos = readonly [number, number];
const asStr = (pos: Pos) => pos.join(",");

const blockMap = new Set<string>();
const patternMap = new Map<string, number>();
let maxY = 0;
let rockI = 0;
let moveI = 0;

let rx = 3;
let ry = maxY + 4;

const newRock = () => {
  rx = 3;
  ry = maxY + 4;
  rockI++;
};

const currentRock = () =>
  rocks[rockI % rocks.length].map(([x, y]) => [x + rx, y + ry] as const);

function drop() {
  ry--;
  if (currentRock().find(([x, y]) => y === 0 || blockMap.has(asStr([x, y])))) {
    ry++;
    currentRock().forEach(([x, y]) => {
      maxY = Math.max(y, maxY);
      blockMap.add(asStr([x, y]));
    });
    newRock();
    return false;
  }
  return true;
}

function shift() {
  const move = moves[moveI++ % moves.length];
  const dx = move === ">" ? 1 : -1;
  rx += dx;
  if (
    currentRock().find(
      ([x, y]) => x === 0 || x === 8 || blockMap.has(asStr([x, y]))
    )
  ) {
    rx -= dx;
  }
}

const debug = () => {
  const str = _.range(maxY + 4, 0)
    .map(
      (y) =>
        "|" +
        _.range(1, 8)
          .map((x) =>
            blockMap.has(asStr([x, y]))
              ? "#"
              : currentRock().find(([xx, yy]) => x === xx && y === yy)
              ? "@"
              : "."
          )
          .join("") +
        "|"
    )
    .concat("+-------+")
    .join("\n");
  console.log(str);
};

const heights = [0];
let part2;

while (rockI < 2022 || !part2) {
  do {
    shift();
  } while (drop());

  let minY = maxY;
  const pattern = _.range(1, 8)
    .map((x) => {
      let y = maxY;
      while (!blockMap.has(asStr([x, y])) && y > 0) {
        y--;
      }
      minY = Math.min(y, minY);
      return y;
    })
    .map((y) => y - minY)
    .concat(rockI % rocks.length, moveI % moves.length)
    .join(",");

  if (!part2 && patternMap.has(pattern)) {
    const absurd = 1000000000000;

    const from = patternMap.get(pattern)!;
    const loopSize = rockI - from - 1;
    const heightChange = heights[rockI - 1] - heights[from];
    const repeats = Math.floor((absurd - from) / loopSize);
    const remaining = absurd - repeats * loopSize;
    part2 = repeats * heightChange + heights[remaining];
  }
  heights.push(maxY);
  patternMap.set(pattern, rockI - 1);
}
console.log(heights[2022]);
console.log(part2);
