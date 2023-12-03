import fs from "fs";

const file = fs.readFileSync("./input.txt");
// const file = fs.readFileSync("./example.txt");

const input = file.toString().trim();

const data = input.split("\n").map((line) => line.split(""));

let [x, y, total] = [0, 0, 0];

const next = () => {
  x += 1;
  if (x === data[0].length) {
    x = 0;
    y += 1;
  }
};

const isDigit = (c: string) => !isNaN(parseInt(c));

// For part 2, tracks gear values - key is 'x,y' value is list of adjacent numbers
const gearMap = new Map<string, number[]>();
const registerGear = (coord: [number, number], val: number) => {
  const key = coord.toString();
  gearMap.get(key)?.push(val) ?? gearMap.set(key, [val]);
};

while (y < data.length) {
  if (!isDigit(data[y][x])) {
    next();
    continue;
  }
  const [startX, startY] = [x, y];
  let numStr = "";

  // Advance until we either find a non-digit or hit the end of the line
  while (isDigit(data[y][x]) && y === startY) {
    numStr += data[y][x];
    next();
  }
  const num = parseInt(numStr);
  const endX = startX + numStr.length;
  const endY = startY + 1;

  let isPart = false;
  // Check the 'box' around the found number
  for (let yy = startY - 1; yy <= endY; yy++) {
    for (let xx = startX - 1; xx <= endX; xx++) {
      const val = data[yy]?.[xx];
      // Look for a non-digit, non . to see if the number is a part
      if (val && val !== "." && !isDigit(val)) {
        isPart = true;
      }
      // Look for * to indicate a gear (part 2)
      if (val === "*") {
        registerGear([xx, yy], num);
      }
    }
  }
  if (isPart) total += num;
}
console.log(`Part 1 ${total}`);

// Part 2
const gearTotal = Array.from(gearMap.values())
  .filter(({ length }) => length == 2)
  .map(([x, y]) => x * y)
  .reduce((a, b) => a + b);
console.log(`Part 2 ${gearTotal}`);
