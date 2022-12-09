const fs = require("fs");
const _ = require("lodash");

//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
const [setup, moves] = fs.readFileSync(path).toString().trimEnd().split("\n\n");

const setupLines = setup.split("\n");

const parsedMoves = moves
  .split("\n")
  .map((move) => move.match(/move (.+) from (.+) to (.+)/).slice(1));

const buildStacks = () => {
  let stacks = [];
  for (const line of setupLines.slice(0, -1)) {
    const items = _.chunk(line, 4).map((c) => c[1]);
    items.forEach((item, i) => {
      if (!stacks[i]) stacks[i] = [];
      if (item !== " ") {
        stacks[i].unshift(item);
      }
    });
  }
  return stacks;
};

const solve = (isPart1) => {
  const stacks = buildStacks();
  for (const [count, from, to] of parsedMoves) {
    if (isPart1) {
      for (let i = 0; i < count; i++) {
        stacks[to - 1].push(stacks[from - 1].pop());
      }
    } else {
      const popped = stacks[from - 1].splice(stacks[from - 1].length - count);
      stacks[to - 1] = stacks[to - 1].concat(popped);
    }
  }
  return stacks.map((x) => x[x.length - 1]).join("");
};

console.log(solve(true)); // part 1
console.log(solve(false)); // part 2
