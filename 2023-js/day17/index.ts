import fs from "fs";

import {
  Coord,
  getBounds,
  mapFromInput,
  move,
  renderFromMap,
  xy,
} from "../coordlib/coord.js";
import { Dir, all as allDirs, reverse } from "../coordlib/dir.js";
import { PriorityQueue } from "./PriorityQueue.js";
// import chalk from "chalk";

const input = fs.readFileSync("input.txt").toString().trim();

const map = mapFromInput(input, Number);

type StepHistory = [
  Dir | undefined,
  Dir | undefined,
  Dir | undefined,
  Dir | undefined,
  Dir | undefined,
  Dir | undefined,
  Dir | undefined,
  Dir | undefined,
  Dir | undefined
];

const countStepsInSameDir = (stepHistory: StepHistory) => {
  const dir = stepHistory[0];
  let count = 1;
  for (let i = 1; i < stepHistory.length; i++) {
    if (stepHistory[i] === dir) count++;
    else break;
  }
  return count;
};

type SearchState = {
  coord: Coord;
  totalCost: number;
  recentSteps: StepHistory;
  fullPath: Coord[];
};

const origin = xy(0, 0);
const bounds = getBounds();
const goal = xy(bounds.maxX, bounds.maxY);

class VisitedState {
  nodeMap = new Map<Coord, Map<string, number>>([]);

  private static stringifyRecentSteps(recentSteps: StepHistory) {
    return `${recentSteps[0] ?? "s"}${countStepsInSameDir(recentSteps)}`;
  }

  get(coord: Coord, recentSteps: StepHistory): number | undefined {
    const map = this.nodeMap.get(coord);
    if (!map) return;
    if (!recentSteps) return Math.min(...map.values());
    return map.get(VisitedState.stringifyRecentSteps(recentSteps));
  }
  set(coord: Coord, recentSteps: StepHistory, newCost: number) {
    const map = this.nodeMap.get(coord);
    const key = VisitedState.stringifyRecentSteps(recentSteps);
    if (!map) {
      this.nodeMap.set(coord, new Map([[key, newCost]]));
      return;
    }
    map.set(key, newCost);
  }

  debug() {
    console.clear();
    renderFromMap(this.nodeMap, (mapping) =>
      (mapping ? Math.min(...mapping.values()).toString() : "").padEnd(3)
    );
  }
}

const solve = (
  getAllowedDirs: (stepHistory: StepHistory) => readonly Dir[],
  canStop: (stepHistory: StepHistory) => boolean
) => {
  const visitedState = new VisitedState();

  const frontier = new PriorityQueue<SearchState>([
    [
      0,
      {
        coord: origin,
        totalCost: 0,
        recentSteps: Array.from({ length: 10 }) as StepHistory,
        fullPath: [],
      },
    ],
  ]);

  let state: SearchState;
  let foundAnswer = Infinity;
  let foundPath: Coord[];
  let count = 0;
  while ((state = frontier.pop()!)) {
    // visitedState.debug();
    count++;
    const { coord, totalCost, recentSteps } = state;
    if (coord === goal && canStop(recentSteps)) {
      if (totalCost < foundAnswer) {
        foundAnswer = totalCost;
        foundPath = state.fullPath.concat([goal]);
      }
      break;
    }
    if (totalCost > foundAnswer) continue;

    const allowedDirs = getAllowedDirs(recentSteps);
    for (const d of allowedDirs) {
      const toCheck = move(coord, d);
      const cost = map.get(toCheck);
      if (cost === undefined || toCheck === origin) continue;
      const newRecentSteps = [
        d,
        ...recentSteps.slice(0, recentSteps.length - 1),
      ] as unknown as StepHistory;
      const newCost = totalCost + cost;

      const prevCost = visitedState.get(toCheck, newRecentSteps) ?? Infinity;

      if (prevCost <= newCost) continue;

      if (newCost < prevCost) {
        visitedState.set(toCheck, newRecentSteps, newCost);
      }
      frontier.insert(newCost, {
        coord: toCheck,
        recentSteps: newRecentSteps,
        totalCost: newCost,
        fullPath: state.fullPath.concat([coord]),
      });
    }
  }
  // renderFromMap(map, (tile, coord) =>
  //   foundPath.includes(coord)
  //     ? chalk.blueBright(tile!.toString())
  //     : tile?.toString() ?? " "
  // );
  return foundAnswer;
};

const part1 = solve(
  (steps) => {
    const forbiddenDir =
      countStepsInSameDir(steps) === 3 ? steps[0] : undefined;
    return allDirs.filter(
      (d) => d !== forbiddenDir && d !== reverse[steps[0]!]
    );
  },
  () => true
);
console.log(part1);

const part2 = solve(
  (steps) => {
    const heading = steps[0];
    if (!heading) return allDirs;
    const stepsInSameDir = countStepsInSameDir(steps);
    if (stepsInSameDir < 4) return [heading];
    const forbiddenDir =
      countStepsInSameDir(steps) === 10 ? heading : undefined;
    return allDirs.filter((d) => d !== forbiddenDir && d !== reverse[heading!]);
  },
  (steps) => countStepsInSameDir(steps) >= 4
);
console.log(part2);

// console.log(solve(true));
