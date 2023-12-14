import fs from "fs";
import _ from "lodash";

const inputFile = fs.readFileSync("input.txt").toString().trim();

type SolvedEntry = "#" | ".";
type Entry = "?" | SolvedEntry;

const data = inputFile.split("\n").map((l): [Entry[], number[]] => {
  const [data, key] = l.split(" ");
  return [data.split("") as Entry[], key.split(",").map(Number)];
});

function generatePossibilities(data: Entry[]): SolvedEntry[][] {
  if (data.length === 0) return [[]];
  const [head, ...tail] = data;
  return generatePossibilities(tail).flatMap((newTail): SolvedEntry[][] => {
    return head === "?"
      ? [
          [".", ...newTail],
          ["#", ...newTail],
        ]
      : [[head, ...newTail]];
  });
}

const checkMatch = (_pattern: number[]) => (data: SolvedEntry[]) => {
  const pattern = [..._pattern];
  const trimWorking = () => (data = _.dropWhile(data, (x) => x === "."));
  const countBroken = () => {
    const { length } = data;
    data = _.dropWhile(data, (x) => x === "#");
    return length - data.length;
  };
  trimWorking();
  while (data.length) {
    const count = countBroken();
    const expected = pattern.shift();
    if (expected !== count) return false;
    trimWorking();
  }

  return pattern.length === 0;
};

const countPossibilities = ([data, key]: [Entry[], number[]]) => {
  return generatePossibilities(data).filter(checkMatch(key)).length;
};
console.log(data.map(countPossibilities).reduce((a, b) => a + b));
