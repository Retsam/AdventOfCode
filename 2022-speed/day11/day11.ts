import fs from "fs";
import _ from "lodash";
/*
const path = "input.txt";
/*/
const path = "example.txt";
//*/

const inputFile = fs.readFileSync(path).toString().trim();
const input = inputFile.split("\n\n");

type Monkee = typeof monkees[number];
const monkees = input.map((x) => {
  const [, start, operation, test, tr, fl] = x.match(
    new RegExp(
      /Monkey .+:\s+Starting items: (.+)\s+Operation: new = (.+)\s+Test: divisible by (.+)\s+If true: throw to monkey (.+)\s+If false: throw to monkey (.+)/,
      "m"
    )
  )!;
  return {
    items: start.split(", ").map((x) => parseInt(x)),
    operation: operation.trim(),
    test: parseInt(test),
    onTrue: parseInt(tr),
    onFalse: parseInt(fl),
    count: 0,
  };
});

const allItems = monkees.flatMap((monkee, i) =>
  monkee.items.map((item) => ({
    item,
    startingMonkeeIdx: i,
  }))
);
const startingMonkees = JSON.parse(JSON.stringify(monkees));

function evaluate(old: number, operation: string) {
  // Forgive me for I have sinned
  return eval(`const old = ${old}; ${operation}`) as number;
}

const inspectAndThrow = (item: number, monkee: Monkee, partOne: boolean) => {
  let newWorry = evaluate(item, monkee.operation);
  if (partOne) newWorry = Math.floor(newWorry / 3);
  const nextMonkeeIdx =
    newWorry % monkee.test === 0 ? monkee.onTrue : monkee.onFalse;
  return [newWorry, nextMonkeeIdx] as const;
};

_.times(20, () => {
  for (const monkee of monkees) {
    monkee.count += monkee.items.length;
    while (monkee.items.length) {
      const item = monkee.items.shift()!;
      const [newWorry, nextMonkeeIdx] = inspectAndThrow(item, monkee, true);
      monkees[nextMonkeeIdx].items.push(newWorry);
    }
  }
});
console.log(
  monkees
    .map(({ count }) => count)
    .sort((a, b) => b - a)
    .slice(0, 2)
    .reduce((a, b) => a * b)
);

// Part 2
const totalRounds = 4;
const monkeeCounts = Array.from(monkees, () => 0);

for (const { item, startingMonkeeIdx } of allItems) {
  let [worry, monkeeIdx] = [item, startingMonkeeIdx];
  const rounds: number[][] = [];
  let loopStartIdx = -1;
  do {
    const round = [];
    for (let i = 0; i < monkees.length; i++) {
      if (i === monkeeIdx) {
        round.push(i);
        [worry, monkeeIdx] = inspectAndThrow(worry, monkees[monkeeIdx], false);
      }
    }
    rounds.push(round);
    loopStartIdx = rounds.findIndex((round) => round[0] === monkeeIdx);
  } while (loopStartIdx === -1);
  const loopSize = rounds.length - loopStartIdx;
  const roundMonkeeCounts = Array.from(monkees, () => 0);
  rounds.forEach((round, i) => {
    const roundTimes =
      i > totalRounds
        ? 0
        : i < loopStartIdx
        ? 1
        : Math.ceil((totalRounds - i) / loopSize);
    for (const monkeeIdx of round) {
      roundMonkeeCounts[monkeeIdx] += roundTimes;
      monkeeCounts[monkeeIdx] += roundTimes;
    }
  });
  console.log({ rounds, loopStartIdx, roundMonkeeCounts });
  // const alreadyHeldIdx = monkeePath.indexOf(monkeeIdx);
  // if (alreadyHeldIdx >= 0) {
  //   console.log(monkeePath.concat(monkeeIdx));
  //   const loopSize = monkeePath.length - alreadyHeldIdx;

  //   for (let i = 0; i < monkeePath.length; i++) {
  //     if (i < alreadyHeldIdx) {
  //       monkeeCounts[i]++;
  //     } else {
  //       monkeeCounts[i] += Math.ceil((rounds - i) % loopSize);
  //     }
  //   }
  //   break;
  // }
  // monkeePath.push(monkeeIdx);
}
console.log(monkeeCounts);
