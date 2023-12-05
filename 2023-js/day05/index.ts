import fs from "fs";
import _ from "lodash";

const file = fs.readFileSync("input.txt");

type Range = readonly [number, number];

const parseNumList = (x: string) => x.split(" ").map((x) => parseInt(x));

const [_seeds, ..._maps] = file.toString().trim().split("\n\n");
const seeds = parseNumList(_seeds.slice(7));
const maps = _maps.map((m) => m.split("\n").slice(1).map(parseNumList));

const convert = (input: number, maps: number[][]) =>
  maps
    .map(([dest, src, len]) => {
      if (input >= src && input < src + len) {
        return input - src + dest;
      }
    })
    .find((x) => x !== undefined) ?? input;

const runMaps = (input: number) => maps.reduce(convert, input);

const part1 = Math.min(...seeds.map(runMaps));
console.log(part1);

// Takes a set of ranges [start, end] and for each range and coverts them:
// at each step a range may split into more ranges
const runMapRange = (inputs: Range[]) => {
  return maps.reduce(
    (ranges, map) => ranges.flatMap((range) => convertRange(range, map)),
    inputs
  );
};

// Takes one range and one mapping as input and produces one or more ranges as output`
const convertRange = ([start, end]: Range, map: number[][]): Range[] => {
  // Find all the 'break' points where the input range is broken by a new mapping
  const breaks = map
    .flatMap(([dest, src]) => [dest, src])
    .filter((boundary) => boundary > start && boundary < end);
  // takes [start, end] and [break1, break2] and produces [[start, break1], [break1, break2], [break2, end]]
  const newRanges = _.chunk([start, ...[...breaks, ...breaks].sort(), end], 2);
  // Individually translate the new set of ranges
  return newRanges.map(([start, end]) => {
    const len = end - start;
    const newStart = convert(start, map);
    // Since we already found breakpoints, the end is just calculated from the start
    return [newStart, newStart + len];
  });
};

const part2Seeds = _.chunk(seeds, 2).map(([a, b]) => [a, a + b] as const);
const part2 = Math.min(...runMapRange(part2Seeds).map((x) => x[0]));
console.log(part2);
