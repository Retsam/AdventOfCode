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

const input = fs.readFileSync("input.txt").toString().trim();

const map = mapFromInput(input, Number);

type StepHistory = [Dir?, Dir?, Dir?];
type SearchState = {
  coord: Coord;
  totalCost: number;
  recentSteps: StepHistory;
};

const origin = xy(0, 0);
const bounds = getBounds();
const goal = xy(bounds.maxX, bounds.maxY);

// Calculate the optimal distance from each space
const heuristicMap = new Map<Coord, number>([[goal, map.get(goal)!]]);
for (let x = bounds.maxX; x >= 0; x--) {
  for (let y = bounds.maxY; y >= 0; y--) {
    const coord = xy(x, y);
    if (coord === goal) continue;
    const val = map.get(coord)!;
    const rightNeighbor = heuristicMap.get(xy(x + 1, y)) ?? Infinity;
    const downNeighbor = heuristicMap.get(xy(x, y + 1)) ?? Infinity;
    heuristicMap.set(coord, val + Math.min(rightNeighbor, downNeighbor));
  }
}

class VisitedState {
  nodeMap = new Map<Coord, Map<string, number>>([]);

  private static stringifyRecentSteps(recentSteps: StepHistory) {
    const dir = recentSteps[recentSteps.length - 1];
    let count = 1;
    for (let i = recentSteps.length - 2; i >= 0; i--) {
      if (recentSteps[i] === dir) count++;
      else break;
    }
    return `${dir ?? "s"}${count}`;
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

const solve = () => {
  const visitedState = new VisitedState();

  const frontier = new PriorityQueue<SearchState>([
    [0, { coord: origin, totalCost: 0, recentSteps: [] }],
  ]);

  let state: SearchState;
  let foundAnswer = Infinity;
  let count = 0;
  while ((state = frontier.pop()!)) {
    count++;
    const { coord, totalCost, recentSteps } = state;
    if (coord === goal) {
      foundAnswer = Math.min(totalCost, foundAnswer);
      continue;
    }
    if (totalCost > foundAnswer) continue;
    const [r1, r2, r3] = recentSteps;
    const forbiddenDir = r1 && r1 === r2 && r1 === r3 ? r1 : undefined;

    for (const d of allDirs) {
      if (d === reverse[r3!] || d === forbiddenDir) continue;

      const toCheck = move(coord, d);
      const cost = map.get(toCheck);
      if (cost === undefined || toCheck === origin) continue;
      const newRecentSteps: StepHistory = [r2, r3, d];
      const newCost = totalCost + cost;

      const prevCost = visitedState.get(toCheck, newRecentSteps) ?? Infinity;

      if (prevCost <= newCost) continue;

      if (newCost < prevCost) {
        visitedState.set(toCheck, newRecentSteps, newCost);
      }
      const heuristic = heuristicMap.get(coord)!;
      frontier.insert(newCost + heuristic, {
        coord: toCheck,
        recentSteps: [r2, r3, d],
        totalCost: newCost,
      });
    }
  }
  return foundAnswer;
};
console.log(solve());
