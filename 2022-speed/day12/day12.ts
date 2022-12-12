import fs from "fs";
//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/

const inputFile = fs.readFileSync(path).toString().trim();
const input = inputFile.split("\n");

// Position utilities

type Pos = [number, number];
const asStr = (pos: Pos) => pos.join(",");

const getNeighbors = ([x, y]: Pos): Pos[] => [
  [x - 1, y],
  [x + 1, y],
  [x, y - 1],
  [x, y + 1],
];
const inBounds = ([x, y]: Pos) =>
  x >= 0 && y >= 0 && x < input[0].length && y < input.length;

function heightVal(p: Pos) {
  let x = input[p[1]][p[0]];
  if (x === "S") return 0;
  if (x === "E") return 25;
  return x.charCodeAt(0) - "a".charCodeAt(0);
}

// Solution

const connectionsMap = new Map<string, Pos[]>();
const lowestPoints: Pos[] = [];
let bestSolution = Infinity;

let startPos!: Pos;
let destPos: Pos;

// Traverse the input, calculate all connections, find the starting positions, and ending position
for (let y = 0; y < input.length; y++) {
  for (let x = 0; x < input[y].length; x++) {
    const pos: Pos = [x, y];
    if (input[y][x] === "E") destPos = pos;
    if (input[y][x] === "S") startPos = pos;
    else if (heightVal(pos) === 0) lowestPoints.push(pos);
    connectionsMap.set(
      asStr(pos),
      getNeighbors(pos)
        .filter(inBounds)
        .filter((other) => heightVal(other) - heightVal(pos) < 2)
    );
  }
}

function findShortestPath(startPos: Pos) {
  const shortestPathTo = new Map<string, number>();

  let searchList: Array<[Pos, number]> = [[startPos, 0]];
  while (searchList.length) {
    const [pos, steps] = searchList.pop()!;
    // If this is worse than a previous solution, give up
    if (steps > bestSolution) continue;

    const others = connectionsMap.get(asStr(pos))!.filter((other) => {
      if (steps + 1 < (shortestPathTo.get(asStr(other)) ?? Infinity)) {
        shortestPathTo.set(asStr(other), steps + 1);
        return true;
      } else {
        return false;
      }
    });
    searchList.unshift(...others.map((o): [Pos, number] => [o, steps + 1]));
  }

  // Return the shortest path we found to the destination: might not have one if we gave up due to exceeding
  //   bestSolution on all paths
  return shortestPathTo.get(asStr(destPos)) ?? Infinity;
}

const p1 = findShortestPath(startPos);
console.log(p1);

bestSolution = p1;
for (const lowestPoint of lowestPoints) {
  const res = findShortestPath(lowestPoint);
  bestSolution = Math.min(res, bestSolution);
}
console.log(bestSolution);
