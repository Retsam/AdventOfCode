import fs from "fs";

const data = fs.readFileSync(process.stdin.fd).toString().trim();

const entries = data.split(",");

const hash = (input: string) =>
  input
    .split("")
    .map((c) => c.charCodeAt(0))
    .reduce((total, charCode) => ((total + charCode) * 17) % 256, 0);
const part1 = entries.map(hash).reduce((a, b) => a + b);

console.log(part1);

// Part 2

type Label = string;
type Lens = number;
// Using a map because they retain insertion order, which is relevant
type Box = Map<Label, Lens>;

// Everyone loves sparse arrays!
const boxes: Box[] = [];

const getBox = (box: number) => {
  return boxes[box] ?? (boxes[box] = new Map());
};
const addLens = (box: number, label: string, focalLength: number) => {
  getBox(box).set(label, focalLength);
};
const removeLens = (box: number, label: string) => {
  getBox(box).delete(label);
};

for (let line of entries) {
  const [label, maybeFocalLength] = line.split(/[=-]/);
  const box = hash(label);
  if (maybeFocalLength === "") {
    removeLens(box, label);
  } else {
    addLens(box, label, Number(maybeFocalLength));
  }
}

const part2 = boxes
  .flatMap((map, box) =>
    Array.from(map.entries()).map(([_, lens], i) => (box + 1) * (i + 1) * lens)
  )
  .reduce((a, b) => a + b);
console.log(part2);
