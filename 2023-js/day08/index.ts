import fs from "fs";
import _ from "lodash";

// const input = fs.readFileSync("example.txt").toString().trim();
const input = fs.readFileSync("input.txt").toString().trim();

const [instructions, _mapping] = input.split("\n\n");

const mapping = Object.fromEntries(
  _mapping.split("\n").map((line) => {
    const [src, dests] = line.split(" = ");
    const dest1 = dests.slice(1, 4);
    const dest2 = dests.slice(6, 9);
    return [src, [dest1, dest2]] as const;
  })
);

const buildWalker /*TexasRanger*/ = (startPos: string) => {
  let pos = startPos;
  let count = 0;
  return {
    walkUntil(isAtEnd: (pos: string) => boolean) {
      do {
        let ins = instructions[count % instructions.length];
        pos = mapping[pos][ins === "L" ? 0 : 1];
        count++;
      } while (!isAtEnd(pos));
      return count;
    },
  };
};

console.log(buildWalker("AAA").walkUntil((p) => p === "ZZZ"));

const gcd = (a: number, b: number): number => (b == 0 ? a : gcd(b, a % b));
const lcm = (a: number, b: number): number => (a / gcd(a, b)) * b;

const isAtZ = (p: string) => p.endsWith("Z");

const part2 = Object.keys(mapping)
  .filter((p) => p.endsWith("A"))
  .map(buildWalker)
  // Count the cycle length between two visits to a "Z" space
  // Due to the particular structure of the input:
  //   * Each walker only ever visits the same Z node
  //   * The Z node is the last node it visits before going back to the first node
  //   * It happens to do so at the end of a complete round of instructions
  // If any of these weren't the case this solution wouldn't work
  //   (Yes, I'm a little bit salty about this problem)
  .map((walker) => {
    const cycleStart = walker.walkUntil(isAtZ);
    const cycleEnd = walker.walkUntil(isAtZ);
    return cycleEnd - cycleStart;
  })
  .reduce(lcm, 1);
console.log(part2);
