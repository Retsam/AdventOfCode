import fs from "fs";
import chalk from "../node_modules/chalk/source/index";

type Tile = "." | "S" | "F" | "7" | "L" | "J" | "|" | "-";

const allDirs = ["u", "d", "l", "r"] as const;
type Dir = (typeof allDirs)[number];
type Coord = [number, number];

// prettier-ignore
const connectionsForTile: Record<Tile, Dir[]> = {
  "S": ["u", "l", "d", "r"],
  "|": ["u", "d"],
  "-": ["l", "r"],
  "F": ["r", "d"],
  "7": ["l", "d"],
  "J": ["u", "l"],
  "L": ["u", "r"],
  ".": []
};
const offsets: Record<Dir, Coord> = {
  u: [0, -1],
  r: [1, 0],
  d: [0, 1],
  l: [-1, 0],
};
const reverse: Record<Dir, Dir> = { u: "d", d: "u", l: "r", r: "l" };
const turnLeft: Record<Dir, Dir> = { l: "d", d: "r", r: "u", u: "l" };
const turnRight: Record<Dir, Dir> = { l: "u", d: "l", r: "d", u: "r" };

const moveCoords = ([x, y]: Coord, dir: Dir): Coord => {
  const [dx, dy] = offsets[dir];
  return [x + dx, y + dy];
};

const input = fs.readFileSync(process.stdin.fd).toString().trim();

const map = input.split("\n").map((line) => line.split("") as Tile[]);

const getAt = ([x, y]: Coord) => map[y]?.[x] ?? ".";
const setAt = ([x, y]: Coord, tile: Tile) => (map[y][x] = tile);

const allCoords = map.flatMap((row, y) => row.map((_, x): Coord => [x, y]));

const startCoords = allCoords.find((coord) => getAt(coord) === "S");
if (!startCoords) throw new Error("Failed to find start location");

// Loop and eliminate any pipes that don't have two connections
//  Need to repeat until we stop seeing changes as removing one pipe can cause a previous to become disconnected
let madeChanges = false;
do {
  madeChanges = false;
  for (const coords of allCoords) {
    const pipe = getAt(coords);
    if (pipe === "." || pipe === "S") continue;

    const connections = connectionsForTile[pipe].filter((d) =>
      connectionsForTile[getAt(moveCoords(coords, d))].includes(reverse[d])
    );
    if (connections.length !== 2) {
      setAt(coords, ".");
      madeChanges = true;
    }
  }
} while (madeChanges);

// Starting from the startCoords, step through the adjacent pipes to find the main loop
const mainLoopSet = new Set<string>([startCoords.toString()]);

// Since S isn't directional, need to find the starting set by looking for
// pipes that connect back.
const toCheck: Coord[] = allDirs
  .filter((d) =>
    connectionsForTile[getAt(moveCoords(startCoords, d))].includes(reverse[d])
  )
  .map((d) => moveCoords(startCoords, d));

while (toCheck.length) {
  const coord = toCheck.shift()!;
  if (mainLoopSet.has(coord.toString())) continue;
  mainLoopSet.add(coord.toString());
  toCheck.push(
    ...connectionsForTile[getAt(coord)].map((d) => moveCoords(coord, d))
  );
}

/*
    Trace through the loop path, all empty spaces to the right (clockwise) and left (counter-clockwise)
    of our path.  One of these sides will be the inside and the other the outside
*/
const leftSet = new Set<string>();
const rightSet = new Set<string>();
const markSpaceIfEmpty = (p: Coord, set: Set<string>) => {
  if (!mainLoopSet.has(p.toString())) set.add(p.toString());
};

let pos = startCoords;
// Find a valid starting direction
let heading = allDirs.find((d) =>
  connectionsForTile[getAt(moveCoords(startCoords, d))].includes(reverse[d])
)!;

do {
  // Take a step, mark the left and right spots (if empty) into the appropriate sets)
  pos = moveCoords(pos, heading);
  markSpaceIfEmpty(moveCoords(pos, turnLeft[heading]), leftSet);
  markSpaceIfEmpty(moveCoords(pos, turnRight[heading]), rightSet);
  if (getAt(pos) !== "|" && getAt(pos) !== "-") {
    // Change our heading based on the new pipe, then check the left and right again
    //  (When we go around a corner, we may need to mark both spaces on the outside)
    heading = connectionsForTile[getAt(pos)].find(
      (d) => d !== reverse[heading]
    )!;
    markSpaceIfEmpty(moveCoords(pos, turnLeft[heading]), leftSet);
    markSpaceIfEmpty(moveCoords(pos, turnRight[heading]), rightSet);
  }
} while (pos.toString() !== startCoords.toString());

const [outsideSet, insideSet] =
  leftSet.size < rightSet.size ? [rightSet, leftSet] : [leftSet, rightSet];

function flood(coord: Coord) {
  allDirs
    .map((d) => moveCoords(coord, d))
    .forEach((coord) => {
      if (
        !insideSet.has(coord.toString()) &&
        !mainLoopSet.has(coord.toString())
      ) {
        insideSet.add(coord.toString());
        flood(coord);
      }
    });
}

const parseCoord = (str: string) =>
  str.split(",").map((x) => parseInt(x)) as Coord;

Array.from(insideSet).map(parseCoord).forEach(flood);

console.log(
  map
    .map((l, y) =>
      l
        .map((c, x) =>
          insideSet.has(`${x},${y}`)
            ? "I"
            : outsideSet.has(`${x},${y}`)
            ? "O"
            : c === "."
            ? chalk.redBright(".")
            : chalk.blue(c)
        )
        .join("")
    )
    .join("\n")
);

// Max distance is just half the count of the
console.log("Part 1: " + mainLoopSet.size / 2);
console.log("Part 2: " + insideSet.size);
