const fs = require("fs");
const path = "input.txt";
const input = fs.readFileSync(path).toString().trim().split("\n");

// There are some really clever solutions to this problem involving modulo lookups to determine win and loss.
//    ... this isn't one of them.

const part1Lookup = {
  // choose rock (result + 1)
  X: { A: 4, B: 1, C: 7 },
  // choose paper (result + 2)
  Y: { A: 8, B: 5, C: 2 },
  // scissors (result + 3)
  Z: { A: 3, B: 9, C: 6 },
};

const part2Lookup = {
  // lose (selection)
  X: { A: 3, B: 1, C: 2 },
  // draw (selection + 3)
  Y: { A: 4, B: 5, C: 6 },
  // win (selection + 6)
  Z: { A: 8, B: 9, C: 7 },
};

let part1 = 0;
let part2 = 0;
for (const line of input) {
  const [A, B] = line.split(" ");
  part1 += part1Lookup[B][A];
  part2 += part2Lookup[B][A];
}
console.log(part1);
console.log(part2);
