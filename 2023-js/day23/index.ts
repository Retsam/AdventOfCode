import fs from "fs";
import util from "util";
// import chalk from "chalk";

import {
  Coord,
  getBounds,
  mapFromInput,
  neighbors,
  renderFromMap,
  xy,
} from "../coordlib/coord.js";

const log = (data: unknown) =>
  console.log(
    util.inspect(data, { maxArrayLength: Infinity, depth: Infinity })
  );

const input = fs.readFileSync("input.txt").toString().trim();

const map = mapFromInput(input, (char) => char);

const { minX, minY, maxX, maxY } = getBounds();

const goal = xy(maxX - 1, maxY);

type Region = { id: string; length: number; goesTo: Region[]; isGoal: boolean };

const buildRegionGraph = () => {
  const intersectionMap = new Map<Coord, Region[]>();
  let regionIdCtr = "a".charCodeAt(0);
  const regionForCoord = new Map<Coord, string>();

  const handleRegion = (coord: Coord, prev: Coord = coord): Region => {
    let spaces = [coord];
    let current = coord;
    let regionSize = 0;
    let nextCandidates;
    let regionId = String.fromCharCode(regionIdCtr++);

    while (
      (nextCandidates = neighbors(current).filter(
        (c) => c !== prev && map.has(c) && map.get(c) !== "#"
      )) &&
      nextCandidates.length === 1
    ) {
      prev = current;
      current = nextCandidates[0]!;
      spaces.push(current);
      regionSize++;
    }
    spaces.forEach((c) => regionForCoord.set(c, regionId));

    let intersection = intersectionMap.get(current);

    if (!intersection) {
      intersection = [];
      intersectionMap.set(current, intersection);
      [xy(current[0] + 1, current[1]), xy(current[0], current[1] + 1)]
        .filter((c) => map.get(c) && map.get(c) !== "#")
        .forEach((coord) => {
          const newRegion = handleRegion(coord, current);
          intersection!.push(newRegion);
        });
    }

    const region = {
      id: regionId,
      length: regionSize + 1,
      goesTo: intersection,
      isGoal: current === goal,
    };

    return region;
  };

  return handleRegion(xy(1, 1), xy(1, 0)); // Starting at 1,1 because the starting square isn't counted in the length

  // renderFromMap(map, (tile, coord) =>
  //   regionForCoord.has(coord)
  //     ? regionForCoord.get(coord)!
  //     : coord === start
  //     ? "S"
  //     : coord === goal
  //     ? "E"
  //     : renderTile(tile!)
  // );
};

// const renderTile = (tile: string) =>
//   tile === "." ? chalk.white(tile) : tile === "#" ? chalk.blue(tile) : tile;

const graph = buildRegionGraph();

const findLongestPath = (
  region: Region,
  previous: Region[],
  lengthSoFar: number
): number => {
  if (region.isGoal) return lengthSoFar + region.length;
  const paths = region.goesTo.map((nextRegion) =>
    findLongestPath(
      nextRegion,
      previous.concat(region),
      lengthSoFar + region.length
    )
  );
  return Math.max(0, ...paths);
};

console.log(findLongestPath(graph, [], 0));
