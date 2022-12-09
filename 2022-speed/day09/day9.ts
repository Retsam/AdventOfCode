import fs from "fs";
import _ from "lodash";
import osPath from "path";
//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/

const inputFile = fs.readFileSync(path).toString().trim();
const input = inputFile
  .split("\n")
  .map((l) => l.split(" "))
  .map(([D, N]) => [D, parseInt(N)] as const);

type Pos = { x: number; y: number };

function solve(knotCount: number) {
  const knots = _.times(knotCount, () => ({ x: 0, y: 0 }));

  const posSet = new Set<string>();
  // Mark the position of the last knot
  function markPos() {
    const T = knots[knots.length - 1];
    posSet.add(`${T.x},${T.y}`);
  }

  // Move the "head"
  function step(dir: string) {
    let H = knots[0];
    if (dir === "R") H.x += 1;
    if (dir === "L") H.x -= 1;
    if (dir === "U") H.y -= 1;
    if (dir === "D") H.y += 1;
    for (const T of knots.slice(1)) {
      follow(H, T);
      H = T;
    }

    markPos();
  }

  // T moves towards H, if necessary
  function follow(H: Pos, T: Pos) {
    const dx = H.x - T.x;
    const dy = H.y - T.y;
    if (Math.abs(dx) === 2 || Math.abs(dy) === 2) {
      T.x += Math.sign(dx);
      T.y += Math.sign(dy);
    }
  }

  for (const [dir, count] of input) {
    _.times(count, () => step(dir));
  }
  return posSet.size;
}

console.log(solve(2));
console.log(solve(10));
