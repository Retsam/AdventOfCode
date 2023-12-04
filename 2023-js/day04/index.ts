import fs from "fs";

const file = fs.readFileSync(process.stdin.fd);

const input = file.toString().trim().split("\n");

const cardWinCounts = input.map((line) => {
  // First part is the card number, but we don't need to parse it, it'll just be the index + 1
  line = line.split(":")[1].trim();
  const [winning, hasNums] = line
    .split(" | ")
    .map((section) => section.split(/\s+/));

  // Tested using a set for winning: doesn't improve performance
  return hasNums.filter((num) => winning.includes(num)).length;
});

const part1 = cardWinCounts.reduce((a, b) => a + b);

console.log(part1);

// Since each card depends on cards after it, loop backwards over cards to build the array
let prevCards: number[] = [];
for (const winCount of cardWinCounts.reverse()) {
  prevCards.unshift(
    // 1 since the card always counts itself, then
    1 + prevCards.slice(0, winCount).reduce((a, b) => a + b, 0)
  );
}

console.log(prevCards.reduce((a, b) => a + b));
