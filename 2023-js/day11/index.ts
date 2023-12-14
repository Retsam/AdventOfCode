import fs from "fs";

const range = (start: number, end: number) =>
  Array.from({ length: end - start }, (_, i) => i + start);

const isBetween = (a: number, b: number) => (n: number) => {
  return (a < n && n < b) || (b < n && n < a);
};

const input = fs.readFileSync(process.stdin.fd).toString().trim();
const lines = input.split("\n");

const galaxies = lines.flatMap((line, y) =>
  line.split("").flatMap((char, x) => {
    if (char === "#") return [[x, y] as const];
    return [];
  })
);
const xs = galaxies.map((g) => g[0]);
const ys = galaxies.map((g) => g[1]);
const [minX, maxX] = [Math.min(...xs), Math.max(...xs)];
const [minY, maxY] = [Math.min(...ys), Math.max(...ys)];

const emptyX = range(minX + 1, maxX).filter(
  (x) => !galaxies.find((entry) => entry[0] === x)
);

const emptyY = range(minY + 1, maxY).filter(
  (y) => !galaxies.find((entry) => entry[1] === y)
);

function solve(distanceMultiplier: number) {
  return (
    galaxies
      .flatMap((a) => {
        return galaxies.flatMap((b) => {
          const extraX = emptyX.filter(isBetween(a[0], b[0])).length;
          const extraY = emptyY.filter(isBetween(a[1], b[1])).length;

          return (
            Math.abs(a![0] - b![0]) +
            Math.abs(a![1] - b![1]) +
            // Extra space due to expansion
            // Multiplier minus 1 because we've already counted it once in the normal distance calculation
            extraX * (distanceMultiplier - 1) +
            extraY * (distanceMultiplier - 1)
          );
        });
      })
      // Divide by 2 because we double counted, since we compared both (a,b) and (b,a)
      .reduce((a, b) => a + b) / 2
  );
}

console.log(solve(2));
console.log(solve(1_000_000));
