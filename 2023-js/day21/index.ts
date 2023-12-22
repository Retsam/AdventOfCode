import fs from "fs";
import util from "util";
import { Coord, mapFromInput, neighbors } from "../coordlib/coord.js";

const log = (data: unknown) =>
  console.log(
    util.inspect(data, { maxArrayLength: Infinity, depth: Infinity })
  );

const input = fs.readFileSync("input.txt").toString().trim();

type Tile = "." | "#";
let _startCoords: Coord | undefined;
const map = mapFromInput(input, (char, coord): Tile => {
  if (char === "#") return "#";
  if (char === "S") {
    _startCoords = coord;
  }
  return ".";
});
if (!_startCoords) throw new Error("Expected to have startCoords");
const startCoords = _startCoords;

const visitedSet = new Map<Coord, Set<number>>();
const addVisited = ([coord, dist]: State) => {
  const cache = visitedSet.get(coord);
  if (!cache) visitedSet.set(coord, new Set([dist]));
  else cache.add(dist);
  return;
};
const hasVisited = ([coord, dist]: State) => {
  return visitedSet.get(coord)?.has(dist) ?? false;
};

let answerCount = 0;

type State = [Coord, number];
const searchList: State[] = [[startCoords, 0]];

let toCheck: State;
while ((toCheck = searchList.pop()!)) {
  const [coord, dist] = toCheck;
  if (dist === 64) {
    answerCount++;
    continue;
  }
  const newSearch = neighbors(coord)
    .map((neighbor): State => [neighbor, dist + 1])
    .filter(([coord]) => map.get(coord) === ".")
    .filter((state) => !hasVisited(state));
  newSearch.forEach(addVisited);
  searchList.push(...newSearch);
}

console.log(answerCount);
