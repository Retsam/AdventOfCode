const fs = require("fs");
const nodePath = require("path");

//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
const input = fs
  .readFileSync(nodePath.join(__dirname, path))
  .toString()
  .trim()
  .split("\n");

let fish = input[0].split(",").map((x) => +x);

let fishCounts = fish.reduce((counts, f) => {
  counts[f] = (counts[f] ?? 0) + 1;
  return counts;
}, {});

const tick = () => {
  let newCounts = {};
  function add(t, c) {
    newCounts[t] = (newCounts[t] ?? 0) + c;
  }
  for (const [t, c] of Object.entries(fishCounts)) {
    if (t === "0") {
      add(8, c);
      add(6, c);
    } else {
      add(+t - 1, c);
    }
  }
  fishCounts = newCounts;
};

const countFishes = () => Object.values(fishCounts).reduce((a, b) => a + b);

for (let i = 0; i < 80; i++) {
  tick();
}
console.log(countFishes());
for (let i = 0; i < 256 - 80; i++) {
  tick();
}
console.log(countFishes());
