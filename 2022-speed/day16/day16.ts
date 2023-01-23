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
  me: { valve: string; from?: string };
  ele: { valve: string; from?: string };
  p: number;
  dp: number;
  openValves: Set<string>;
};
const goodIdeas: Pos[] = [
  {
    time: 26,
    me: { valve: "AA" },
    ele: { valve: "AA" },
    p: 0,
    dp: 0,
    openValves: new Set(),
  },
];
const badIdeas: Pos[] = [];

const maxDp = valveData.map((valve) => valve.flow).reduce((a, b) => a + b);
console.log(maxDp);

function newPaths(path: Pos) {
  const newTime = path.time - 1;
  const newP = path.p + path.dp;
  const meValve = valves[path.me.valve];
  const myMoves: Array<["open" | "move", string]> = [];
  if (meValve.flow && !path.openValves.has(meValve.valve)) {
    myMoves.push(["open", meValve.valve]);
  }
  for (const dest of valves[path.me.valve].dests) {
    if (dest === path.me.from) continue;
    myMoves.push(["move", dest]);
  }
  const eleValve = valves[path.ele.valve];
  const eleMoves = [];
  if (eleValve.flow && !path.openValves.has(eleValve.valve)) {
    eleMoves.push(["open", eleValve.valve]);
  }
  for (const dest of valves[path.ele.valve].dests.reverse()) {
    if (dest === path.ele.from) continue;
    eleMoves.push(["move", dest]);
  }
  const newPositions: Pos[] = [];
  for (const [m1, v1] of myMoves) {
    for (const [m2, v2] of eleMoves) {
      let newDp = path.dp;
      let openValves = path.openValves;
      let myPos: { valve: string; from?: string } = {
        valve: path.me.valve,
      };
      if (m1 === "open") {
        openValves = new Set(openValves);
        openValves.add(v1);
        newDp += valves[v1].flow;
      } else {
        myPos = { valve: v1, from: myPos.valve };
      }
      let elePos: { valve: string; from?: string } = { valve: path.ele.valve };
      if (m2 === "open") {
        if (m1 === "open" && v2 === v1) continue;
        openValves = new Set(openValves);
        openValves.add(v2);
        newDp += valves[v2].flow;
      } else {
        elePos = { valve: v2, from: elePos.valve };
      }
      const newPos = {
        p: newP,
        dp: newDp,
        openValves,
        time: newTime,
        me: myPos,
        ele: elePos,
      };
      //   console.log(m1, v1, m2, v2, newPos);

      newPositions.push(newPos);
    }
  }
  return newPositions;
  //   const { flow, dests } = valves[path.valve];
  //   const newPositions: Pos[] = dests
  //     .filter((dest) => dest !== path.from)
  //     .map((d) => ({
  //       ...path,
  //       valve: d,
  //       time: newTime,
  //       p: newP,
  //       from: path.valve,
  //     }));
  //   const [dumbPositions, smartPositions] = _.partition(
  //     newPositions,
  //     ({ valve }) => path.openValves.has(valve)
  //   );
  //   if (!path.openValves.has(path.valve) && flow > 0) {
  //     const newSet = new Set(path.openValves);
  //     newSet.add(path.valve);
  //     smartPositions.push({
  //       time: newTime,
  //       valve: path.valve,
  //       p: newP,
  //       dp: flow + path.dp,
  //       openValves: newSet,
  //     });
  //   }
  //   return [smartPositions, dumbPositions];
}

let best = 1474; // part 1 solution
let c = 0;
while (goodIdeas.length) {
  const next = goodIdeas.pop()!;
  console.log(next);
  if (next.p > best) best = next.p;
  if (next.time === 0 || next.p + next.time * maxDp < best) {
    continue;
  }
  if (c++ > 30) break;
  const smart = newPaths(next);
  goodIdeas.push(...smart);
}

console.log(best);
