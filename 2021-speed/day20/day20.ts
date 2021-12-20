import * as fs from "fs";
import * as nodePath from "path";
import * as _ from "lodash";

//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
let [algo, image] = fs
  .readFileSync(nodePath.join(__dirname, path))
  .toString()
  .trim()
  .split("\n\n");

// Coordinate utils
type Coord = [number, number];

const toStr = (coord: Coord) => JSON.stringify(coord);

const range = (a: number, b: number) =>
  Array.from(new Array(b - a)).map((_, i) => i + a);

const neighbors = ([x, y]: Coord): Coord[] =>
  [-1, 0, 1].flatMap((dx, _, arr) => arr.map<Coord>((dy) => [x + dy, y + dx]));

const bounds = (_data: Set<string>) => {
  const data = Array.from(_data).map((c) => JSON.parse(c) as Coord);
  return {
    xMin: _.min(data.map((c) => c[0])),
    yMin: _.min(data.map((c) => c[1])),
    xMax: _.max(data.map((c) => c[0])),
    yMax: _.max(data.map((c) => c[1])),
  };
};

const diagram = (
  { xMin = 0, xMax, yMin = 0, yMax },
  charFunc: (c: Coord) => string
) =>
  range(yMin, yMax + 1)
    .map((y) =>
      range(xMin, xMax + 1)
        .map((x) => charFunc([x, y]))
        .join("")
    )
    .join("\n");

let imageData = new Set<string>();
image.split("\n").flatMap((line, y) =>
  line.split("").forEach((c, x) => {
    if (c === "#") {
      imageData.add(toStr([x, y]));
    }
  })
);

const calcValue = _.memoize(
  (coord: Coord, generation: number) => {
    if (generation === 0) {
      return imageData.has(toStr(coord)) ? "#" : ".";
    }
    const bits = neighbors(coord)
      .map((n) => (calcValue(n, generation - 1) === "#" ? "1" : "0"))
      .join("");
    return algo[parseInt(bits, 2)];
  },
  (coord, gen) => [coord, gen].join(",")
);

const calculateGeneration = (g: number) => {
  const b = bounds(imageData);
  const img = new Set<string>();
  for (let x = b.xMin - g; x <= b.xMax + g; x++) {
    for (let y = b.yMin - g; y <= b.yMax + g; y++) {
      if (calcValue([x, y], g) === "#") {
        img.add(toStr([x, y]));
      }
    }
  }
  return img;
};

const show = (imageData: Set<string>) => {
  console.log(
    diagram(bounds(imageData), (coord) =>
      imageData.has(toStr(coord)) ? "#" : "."
    )
  );
};

const gen2 = calculateGeneration(2);
const gen50 = calculateGeneration(50);
console.log(gen2.size);
console.log(gen50.size);
