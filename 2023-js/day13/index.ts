import fs from "fs";
import _ from "lodash";

const inputFile = fs.readFileSync(process.stdin.fd).toString().trim();
const groups = inputFile.split("\n\n").map((g) => g.split("\n"));

const range = (n: number) => Array.from({ length: n }, (_, i) => i);

const checkXSymmetry =
  (
    part2: boolean,
    getVal: (x: number, y: number) => string,
    maxX: number,
    maxY: number
  ) =>
  (x: number) => {
    const matchCount = range(maxY).filter((y) => {
      let [x1, x2] = [x, x + 1];
      while (x1 >= 0 && x2 < maxX) {
        if (getVal(x1, y) !== getVal(x2, y)) return false;
        x2 += 1;
        x1 -= 1;
      }
      return true;
    }).length;
    return matchCount === maxY - (part2 ? 1 : 0);
  };

const solve = (part2: boolean) => {
  return groups
    .map((group) => {
      const maxX = group[0].length;
      const maxY = group.length;
      const xSymmetry = range(maxX - 1).find(
        checkXSymmetry(part2, (x, y) => group[y][x], maxX, maxY)
      );
      if (xSymmetry !== undefined) return xSymmetry + 1;
      const ySymmetry = range(maxY - 1).find(
        checkXSymmetry(part2, (x, y) => group[x][y], maxY, maxX)
      );
      if (ySymmetry === undefined) throw new Error("Failed");
      return (ySymmetry + 1) * 100;
    })
    .reduce((a, b) => a + b);
};

console.log(solve(false));
console.log(solve(true));
