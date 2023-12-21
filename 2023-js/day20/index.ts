import fs from "fs";
import util from "util";

const log = (data: unknown) =>
  console.log(
    util.inspect(data, { maxArrayLength: Infinity, depth: Infinity })
  );

type BaseModule = {
  name: string;
  outputs: string[]; // module ids
};
type Module = BaseModule &
  (
    | { kind: "conjunction"; states: Record<string, 0 | 1> }
    | { kind: "flip-flop"; state: 0 | 1 }
    | { kind: "broadcaster" }
  );

const input = fs.readFileSync("input.txt").toString().trim();

const parseModule = (str: string, outputs: string[]): Module => {
  if (str.startsWith("%")) {
    return { name: str.slice(1), kind: "flip-flop", state: 0, outputs };
  }
  if (str.startsWith("&")) {
    return {
      name: str.slice(1),
      kind: "conjunction",
      outputs,
      states: {},
    };
  }
  return { name: str, kind: "broadcaster", outputs };
};

const modules = Object.fromEntries(
  input.split("\n").map((line): [string, Module] => {
    const [input, output] = line.split(" -> ");
    const outputs = output.split(", ");
    const mod = parseModule(input, outputs);
    return [mod.name, mod];
  })
);

for (const module of Object.values(modules)) {
  module.outputs.forEach((mod) => {
    const targetMod = modules[mod];
    if (targetMod?.kind === "conjunction") {
      targetMod.states[module.name] = 0;
    }
  });
}

type Signal = { source: string; target: string; state: 0 | 1 };

const signals: Signal[] = [];
let signalCounts = [0, 0] as [number, number];

function sendSignals(state: 0 | 1, { name, outputs }: BaseModule) {
  signals.push(...outputs.map((target) => ({ source: name, target, state })));
}

function handleSignal(signal: Signal) {
  signalCounts[signal.state]++;
  //   console.log(
  //     `${signal.source} -${signal.state === 0 ? "low" : "high"}-> ${
  //       signal.target
  //     }`
  //   );
  const module = modules[signal.target];
  if (!module) return;

  if (module.kind === "flip-flop") {
    if (signal.state === 1) return;
    const newState = (1 - module.state) as 1 | 0;
    module.state = newState;
    sendSignals(newState, module);
    return;
  }
  if (module.kind === "conjunction") {
    module.states[signal.source] = signal.state;
    const newSignal = Object.values(module.states).every((state) => state === 1)
      ? 0
      : 1;
    sendSignals(newSignal, module);
  }
  if (module.kind === "broadcaster") sendSignals(0, module);
}
let signal: Signal;

let pressCount = 0;
function runCycle() {
  signals.push({ source: "button", target: "broadcaster", state: 0 });
  pressCount += 1;
  while ((signal = signals.shift()!)) {
    handleSignal(signal);
  }
  //   console.log("----");
}

Array.from({ length: 1000 }, (_, i) => i).forEach(() => runCycle());
console.log(signalCounts[0] * signalCounts[1]);
