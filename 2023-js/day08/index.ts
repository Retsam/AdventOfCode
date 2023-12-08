import fs from "fs";
import _ from "lodash";

const input = fs.readFileSync(process.stdin.fd).toString().trim();

const [_instructions, _mapping] = input.split("\n\n");

// The instructions only have L or R and we only need indexing and length checks
const instructions = _instructions as ArrayLike<"L" | "R">;
const mapping = Object.fromEntries(
  _mapping.split("\n").map((line) => {
    const [src, dest1, dest2] = line.split(/\W+/);
    return [src, { L: dest1, R: dest2 }];
  })
);

const buildWalker /*TexasRanger*/ = (startPos: string) => {
  let pos = startPos;
  let count = 0;
  return {
    walkUntil(isAtEnd: (pos: string) => boolean) {
      do {
        pos = mapping[pos][instructions[count % instructions.length]];
        count++;
      } while (!isAtEnd(pos));
      return count;
    },
  };
};

console.log(buildWalker("AAA").walkUntil((p) => p === "ZZZ"));

const gcd = (a: number, b: number): number => (b == 0 ? a : gcd(b, a % b));
const lcm = (a: number, b: number): number => (a / gcd(a, b)) * b;

const part2 = Object.keys(mapping)
  .filter((p) => p.endsWith("A"))
  // Count the cycle length between two visits to a "Z" space, which is the same as the distance
  //  to get to Z the first time, due to the particular structure of the input:
  //   * Each walker only ever visits the same Z node
  //   * The Z node is the last node it visits before going back to the first node
  //   * It happens to do so at the end of a complete round of instructions
  // If any of these weren't the case this solution wouldn't work
  //   (Yes, I'm a little bit salty about this problem)
  .map((p) => buildWalker(p).walkUntil((p) => p.endsWith("Z")))
  .reduce(lcm, 1);
console.log(part2);
