import fs from "fs";

const input = fs.readFileSync("./input.txt");

const lines = input.toString().trim().split("\n");

// prettier-ignore
const digitWords = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

function solve(isPartTwo: boolean) {
  return lines
    .map((line) => {
      const chars = line
        .split("")
        .map((c, i) => {
          if (parseInt(c)) return parseInt(c);
          if (!isPartTwo) return;
          const englishDigit = digitWords.findIndex((word) =>
            line.slice(i).startsWith(word)
          );
          return englishDigit > 0 ? englishDigit : undefined;
        })
        .filter((x) => x);

      return parseInt(`${chars[0]}${chars[chars.length - 1]}`);
    })
    .reduce((a, b) => a + b);
}

console.log(solve(false), solve(true));
