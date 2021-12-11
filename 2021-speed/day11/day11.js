const fs = require("fs");
const nodePath = require("path");
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

let data = input.map((line) => line.split("").map((x) => +x));

const neighbors = (x, y) =>
  [-1, 0, 1]
    .flatMap((dx, _, arr) => arr.map((dy) => [x + dx, y + dy]))
    .filter(([xx, yy]) => x !== xx || y !== yy);

let totalFlashes = 0;
let currentGenerationFlashes = 0;
function flash(data, x, y) {
  totalFlashes += 1;
  currentGenerationFlashes += 1;

  neighbors(x, y).forEach(([xx, yy]) => {
    if (data[xx]?.[yy] === undefined) return;

    data[xx][yy] += 1;
    if (data[xx][yy] === 10) {
      flash(data, xx, yy);
    }
  });
}

let [maxX, maxY] = [data.length, data[0].length];
const step = () => {
  currentGenerationFlashes = 0;
  // Increment, checking for flashes
  for (let x = 0; x < maxX; x++) {
    for (let y = 0; y < maxY; y++) {
      data[x][y]++;
      if (data[x][y] === 10) {
        flash(data, x, y);
      }
    }
  }
  if (currentGenerationFlashes === maxX * maxY) {
    return true;
  }

  // Reset any that flashed
  for (let x = 0; x < maxX; x++) {
    for (let y = 0; y < maxY; y++) {
      if (data[x][y] > 9) {
        data[x][y] = 0;
      }
    }
  }
  return false;
};

let c = 1;
while (!step(data)) {
  if (c === 100) console.log(totalFlashes);
  c++;
}
console.log(c);
