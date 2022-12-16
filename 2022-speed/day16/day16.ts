import fs from "fs";
import _, { forEach } from "lodash";
//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/

const inputFile = fs.readFileSync(path).toString().trim();
const lines = inputFile.split("\n");
const valveData = lines
  .map(
    (line) =>
      // line.match(/Valve \w+ has flow rate=\d+; tunnels? leads to valves?/)
      line.match(
        /Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)/
      )!
  )
  .map(([, valve, flow, dests]) => ({
    valve,
    flow: parseInt(flow),
    dests: dests.split(", "),
  }));

const valves = _.mapValues(_.groupBy(valveData, "valve"), ([x]) => x);

type Pos = {
  time: number;
  valve: string;
  p: number;
  dp: number;
  openValves: Set<string>;
  from?: string;
};
const goodIdeas: Pos[] = [
  { time: 30, valve: "AA", p: 0, dp: 0, openValves: new Set() },
];
const badIdeas: Pos[] = [];

const maxDp = valveData.map((valve) => valve.flow).reduce((a, b) => a + b);
console.log(maxDp);

function newPaths(path: Pos) {
  const newTime = path.time - 1;
  const newP = path.p + path.dp;
  const { flow, dests } = valves[path.valve];
  const newPositions: Pos[] = dests
    .filter((dest) => dest !== path.from)
    .map((d) => ({
      ...path,
      valve: d,
      time: newTime,
      p: newP,
      from: path.valve,
    }));
  const [dumbPositions, smartPositions] = _.partition(
    newPositions,
    ({ valve }) => path.openValves.has(valve)
  );
  if (!path.openValves.has(path.valve) && flow > 0) {
    const newSet = new Set(path.openValves);
    newSet.add(path.valve);
    smartPositions.push({
      time: newTime,
      valve: path.valve,
      p: newP,
      dp: flow + path.dp,
      openValves: newSet,
    });
  }
  return [smartPositions, dumbPositions];
}

let best = 0;
while (goodIdeas.length || badIdeas.length) {
  const next = goodIdeas.pop() ?? badIdeas.pop()!;
  if (next.p > best) best = next.p;
  if (next.time === 0 || next.p + next.time * maxDp < best) {
    continue;
  }
  const [smart, dumb] = newPaths(next);
  goodIdeas.push(...smart);
  badIdeas.push(...dumb);
}

console.log(best);
