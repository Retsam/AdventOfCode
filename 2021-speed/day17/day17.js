const fs = require("fs");
const nodePath = require("path");
const _ = require("lodash");
//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
let data = fs
  .readFileSync(nodePath.join(__dirname, path))
  .toString()
  .trim()
  .slice("target area: ".length)
  .split(", ")
  .map((x) =>
    x
      .slice(2)
      .split("..")
      .map((x) => +x)
  );

const [[minX, maxX], [minY, maxY]] = data;

const succeeds = (vx, vy) => {
  if (vx === 0 && vy === 0) return false;
  let [x, y] = [0, 0];
  let maxHeight = 0;
  while (y > minY && x < maxX) {
    x += vx;
    y += vy;
    vx = Math.max(vx - 1, 0);
    vy -= 1;
    maxHeight = Math.max(y, maxHeight);
    if (x >= minX && x <= maxX && y >= minY && y <= maxY) {
      return maxHeight;
    }
  }
  return false;
};

let mY = 0;
let c = 0;
for (let x = 0; x <= maxX; x++) {
  for (let y = minY; y < 10000; y++) {
    const res = succeeds(x, y);
    if (res !== false) {
      c++;
      console.log(x, y);

      mY = Math.max(mY, res);
    }
  }
}
console.log(c, mY);
