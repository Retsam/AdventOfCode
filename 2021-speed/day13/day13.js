const fs = require("fs");
const nodePath = require("path");
const _ = require("lodash");
//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
const input = fs.readFileSync(nodePath.join(__dirname, path)).toString().trim();

const range = (a, b) => Array.from(new Array(b - a)).map((_, i) => i + a);

let [dots, ins] = input.split("\n\n");
dots = dots.split("\n").map((l) => l.split(",").map((x) => +x));
ins = ins.split("\n").map((i) => {
  const [dir, v] = i.match(/along (.)=(\d+)/).slice(1);
  return [dir, +v];
});

const step = ([foldDir, v]) => {
  dots = dots.map(([x, y]) => {
    if (foldDir === "x" && x > v) {
      return [v + (v - x), y];
    } else if (foldDir === "y" && y > v) {
      return [x, v + (v - y)];
    }
    return [x, y];
  });
};

step(ins[0]);
const uniqueDots = new Set(dots.map((d) => d.join(",")));
console.log(uniqueDots.size);
ins.slice(1).forEach(step);

const [xMax, yMax] = [
  Math.max(...dots.map((d) => d[0])),
  Math.max(...dots.map((d) => d[1])),
];
const output = range(0, yMax + 1)
  .map((y) => {
    return range(0, xMax + 1)
      .map((x) => {
        return dots.find((d) => d[0] === x && d[1] === y) ? "#" : " ";
      })
      .join("");
  })
  .join("\n");

console.log(output);
