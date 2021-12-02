const fs = require("fs");
const nodePath = require("path");

const path = "input.txt";
// const path = "example.txt";
const input = fs.readFileSync(nodePath.join(__dirname, path)).toString().trim();

const data = input.split("\n");

const [h, d] = data.reduce(
  ([horiz, depth, aim], cmd) => {
    const [move, _dist] = cmd.split(" ");
    const dist = +_dist;
    if (move === "up") {
      return [horiz, depth, aim - dist];
    }
    if (move === "down") {
      return [horiz, depth, aim + dist];
    }
    return [horiz + dist, depth + dist * aim, aim];
  },
  [0, 0, 0]
);

console.log(h * d);
