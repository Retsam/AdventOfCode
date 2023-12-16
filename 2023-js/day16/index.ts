import fs from "fs";

const range = (start: number, end: number) =>
  Array.from({ length: end - start }, (_, i) => i + start);

// const input = fs.readFileSync("example.txt").toString().trim();
const input = fs.readFileSync("input.txt").toString().trim();

const data = input.split("\n").map((l) => l.split(""));
const maxX = data[0]!.length;
const maxY = data.length;

type Coord = [number, number];

type Dir = "l" | "r" | "u" | "d";
type Beam = [Coord, Dir];
const offsets: Record<Dir, Coord> = {
  u: [0, -1],
  r: [1, 0],
  d: [0, 1],
  l: [-1, 0],
};
const moveCoords = ([x, y]: Coord, dir: Dir): Coord => {
  const [dx, dy] = offsets[dir];
  return [x + dx, y + dy];
};

const flipForward = {
  r: "u",
  u: "r",
  l: "d",
  d: "l",
} as const;
const flipBack = {
  r: "d",
  d: "r",
  l: "u",
  u: "l",
} as const;

const getAt = ([x, y]: Coord) => data[y]?.[x];

function solve(init: Beam) {
  // Tracks the beam position and direction
  const beamSet = new Set<string>();

  const beams: Beam[] = [init];

  while (beams.length) {
    const [coord, dir] = beams.pop()!;
    const stateStr = [coord, dir].toString();
    if (beamSet.has(stateStr)) continue;
    beamSet.add(stateStr);
    const c2 = moveCoords(coord, dir);
    const tile = getAt(c2);
    if (tile === undefined) continue;
    if (
      tile === "." ||
      (tile === "-" && (dir === "r" || dir === "l")) ||
      (tile === "|" && (dir === "d" || dir === "u"))
    ) {
      beams.unshift([c2, dir]);
      continue;
    }
    if (tile === "|") {
      beams.unshift([c2, "u"]);
      beams.unshift([c2, "d"]);
      continue;
    }
    if (tile === "-") {
      beams.unshift([c2, "l"]);
      beams.unshift([c2, "r"]);
      continue;
    }
    if (tile === "/") {
      beams.unshift([c2, flipForward[dir]]);
      continue;
    }
    if (tile === "\\") {
      beams.unshift([c2, flipBack[dir]]);
      continue;
    }
  }
  const energizedSet = new Set(
    [...beamSet.entries()].map(([e]) => e.slice(0, -2))
  );

  return energizedSet.size - 1; // Don't count the start tile (e.g. (-1, 0))
}

const starts = range(0, maxY)
  .flatMap((y): Beam[] => [
    [[-1, y], "r"],
    [[maxX, y], "l"],
  ])
  .concat(
    range(0, maxX).flatMap((x): Beam[] => [
      [[x, -1], "d"],
      [[x, maxY], "u"],
    ])
  );
const allSolutions = starts.map(solve);

// Ordered the starts so that the first one is [-1, 0]
console.log(allSolutions[0]);
console.log(Math.max(...allSolutions));
