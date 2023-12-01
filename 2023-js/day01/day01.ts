import fs from "fs";

const input = fs.readFileSync("./input.txt");

const lines = input.toString().trim().split("\n");

// prettier-ignore
const digitWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

function solve(isPartTwo: boolean) {
  return lines
    .map((line) => {
      const digits = line
        .split("")
        .map((c, i) => {
          if (parseInt(c)) return parseInt(c);
          if (!isPartTwo) return;
          return (
            // digitWords is structured so the index of the entry plus one is the value of the word (zero is not a valid digit)
            // If findIndex is -1, then returns zero, which is removed by filter
            digitWords.findIndex((word) => line.slice(i).startsWith(word)) + 1
          );
        })
        .filter((x) => x);

      return 10 * digits[0]! + digits.at(-1)!;
    })
    .reduce((a, b) => a + b);
}

console.log(solve(false), solve(true));
