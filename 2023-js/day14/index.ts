import fs from "fs";
import _ from "lodash";

const input = fs.readFileSync("input.txt").toString().trim();

const lines = input.split("\n");

const data = lines.map((line) => {
  return line.split("");
});

const maxX = lines.length;
const maxY = lines[0].length;

// Moves stones from bottom to top
const tilt = () => {
  for (let x = 0; x < maxX; x++) {
    for (let y = 1; y < maxY; y++) {
      const tile = data[y][x];
      if (tile !== "O") continue;
      let target = y;
      while (target > 0 && data[target - 1][x] === ".") {
        target -= 1;
      }
      if (target !== y) {
        data[target][x] = "O";
        data[y][x] = ".";
      }
    }
  }
};

const rotate = () => {
  const originalData = [...data.map((line) => [...line])];
  for (let x = 0; x < maxX; x++) {
    for (let y = 0; y < maxY; y++) {
      data[y][x] = originalData[maxY - x - 1][y];
    }
  }
};

const sum = () => {
  let sum = 0;
  for (let x = 0; x < maxX; x++) {
    for (let y = 0; y < maxY; y++) {
      if (data[y][x] === "O") sum += maxY - y;
    }
  }
  return sum;
};

const hash = () => data.map((x) => x.join("")).join("\n");

const cycle = () => {
  tilt();
  if (count === 0) console.log(sum());
  rotate();
  tilt();
  rotate();
  tilt();
  rotate();
  tilt();
  rotate();
  count++;
};

let prev = hash();

let count = 0;
let prevSet = new Map<string, number>([]);
do {
  prev = hash();
  prevSet.set(prev, count);
  cycle();
} while (!prevSet.has(hash()));
const cycleStart = prevSet.get(hash())!;
const cycleLength = count - cycleStart;

const stepsToTake = (1000000000 - cycleStart) % cycleLength;

for (let i = 0; i < stepsToTake; i++) {
  cycle();
}

console.log(sum());
