import * as _ from "lodash";
import { split } from "lodash";

//*
// input
let [p1Pos, p2Pos] = [7, 1];
/*/
// example
let [p1Pos, p2Pos] = [4, 8];
//*/
type PlayerState = [pos: number, score: number];
type GameState = [p1: PlayerState, p2: PlayerState, isP1Turn: boolean];

type DiceResult = [value: number, times: number];
let count = 0;
const normalDie = (function* () {
  let i = 0;
  while (true) {
    count++;
    yield i + 1;
    i = (i + 1) % 100;
  }
})();
const rollNormalDice = (): DiceResult[] => [
  [normalDie.next().value + normalDie.next().value + normalDie.next().value, 1],
];

// Get a list of all outcomes [3,4,5,4,...9]
const diracDiceRolls = [1, 2, 3].flatMap((x, i, a) =>
  a.flatMap((y) => a.flatMap((z) => x + y + z))
);

// convert to a list of [roll, frequency] pairs, i.e. [[3, 1], [4, 3]... etc
const diracDiceResults = _.map(
  _.groupBy(diracDiceRolls), // returns { 3: [3], 4: [4,4,4], ...}
  (rolls): DiceResult => [rolls[0], rolls.length]
);
const rollDiracDice = () => diracDiceResults;
diracDiceResults;

const move = ([pos, score]: PlayerState, roll: number): PlayerState => {
  let newPos = pos + roll;
  while (newPos > 10) newPos -= 10;
  return [newPos, score + newPos];
};

let loserScore; // part1 only
function playGame(
  rollDice: () => [roll: number, times: number][],
  winAt: number
) {
  const wins = [0, 0];

  const gameStates = new Map<string, number>();
  gameStates.set(JSON.stringify([[p1Pos, 0], [p2Pos, 0], true]), 1);

  const updateState = (newState: GameState, count: number) => {
    const asStr = JSON.stringify(newState);
    gameStates.set(asStr, (gameStates.get(asStr) ?? 0) + count);
  };

  while (gameStates.size > 0) {
    let [stateStr, count] = gameStates.entries().next().value;
    gameStates.delete(stateStr);

    for (const [roll, rollCount] of rollDice()) {
      let state = JSON.parse(stateStr);
      const currentPlayer = state[2] ? 0 : 1;
      state[2] = !state[2];

      state[currentPlayer] = move(state[currentPlayer], roll);
      if (state[currentPlayer][1] >= winAt) {
        wins[currentPlayer] += count * rollCount;
        loserScore = state[1 - currentPlayer][1];
      } else {
        updateState(state, count * rollCount);
      }
    }
  }
  return wins;
}

playGame(rollNormalDice, 1000);
console.log(count * loserScore);

const wins = playGame(rollDiracDice, 21);
console.log(wins[1] > wins[0] ? wins[1] : wins[0]);
