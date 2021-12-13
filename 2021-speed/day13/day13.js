const fs = require("fs");
const nodePath = require("path");
const _ = require("lodash");
//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
const input = fs.readFileSync(nodePath.join(__dirname, path)).toString().trim();

// Plotting util
const range = (a, b) => Array.from(new Array(b - a)).map((_, i) => i + a);
const diagram = ({ xMin = 0, xMax, yMin = 0, yMax }, charFunc) =>
  range(yMin, yMax + 1)
    .map((y) =>
      range(xMin, xMax + 1)
        .map((x) => charFunc([x, y]))
        .join("")
    )
    .join("\n");

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

console.log(
  diagram({ xMax, yMax }, ([x, y]) =>
    dots.find((d) => d[0] === x && d[1] === y) ? "#" : " "
  )
);
