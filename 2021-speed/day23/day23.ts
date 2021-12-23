import * as fs from "fs";
import * as nodePath from "path";
import * as _ from "lodash";

//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
let data = fs
  .readFileSync(nodePath.join(__dirname, path))
  .toString()
  .trim()
  .split("\n");

const hallways: Hallway = Array.from({ length: 11 }).map((x, i) => {
  const mob = data[1][i + 1];
  return mob === "." ? null : (mob as Mob);
});
const rooms: Room[] = [2, 4, 6, 8].map((slot) => ({
  slot,
  spaces: [0, 1].map((space) => {
    const mob = data[2 + space][slot + 1];
    return mob === "." ? null : (mob as Mob);
  }),
}));

type Mob = "A" | "B" | "C" | "D";
type Room = {
  slot: number;
  spaces: Array<Mob | null>;
};
type Hallway = Array<Mob | null>;

const buildRooms = (mobs: Room["spaces"][]) => {
  let slot = 0;
  return mobs.map<Room>((m) => {
    slot += 2;
    return { slot, spaces: m };
  });
};

// const rooms: Room[] = buildRooms([
//   ["B", "A"],
//   ["C", "D"],
//   ["B", "C"],
//   ["D", "A"],
// ]);

type HallPos = { kind: "hall"; slot: number };
type RoomPos = { kind: "room"; index: number; space: number };
type Pos = RoomPos | HallPos;

type State = readonly [Hallway, Room[]];

const hallSpaces = new Set([0, 1, 3, 5, 7, 9, 10]);
const slotForRoomIdx = (i: number) => i * 2 + 2;

function legalMoves(pos: Pos, [hall, rooms]: State): Pos[] {
  if (pos.kind == "room") {
    let legalMoves: Pos[] = [];
    const { slot, spaces } = rooms[pos.index];
    const mob = spaces[pos.space];
    // if (!mob || "ABCD".indexOf(mob) === pos.index) return [];
    if (!mob) return [];
    let goalRoomIdx = "ABCD".indexOf(mob);
    if (goalRoomIdx === pos.index) {
      if (pos.space === 1) return [];
      if (spaces[1] === mob) return [];
    }
    const goalRoom = rooms[goalRoomIdx];
    const goalRoomValid = goalRoom.spaces.every((m) => !m || m === mob);
    let i = pos.space - 1;
    // can exit
    while (i >= 0) {
      if (spaces[i]) return [];
      i--;
    }
    // hall moves
    for (let h = slot; h < 11; h++) {
      if (hall[h]) break;
      if (hallSpaces.has(h)) legalMoves.push({ kind: "hall", slot: h });
      if (h === goalRoom.slot && goalRoomValid) {
        let destSpace = goalRoom.spaces[1] ? 0 : 1;
        return [{ kind: "room", index: goalRoomIdx, space: destSpace }];
      }
    }
    for (let h = slot; h >= 0; h--) {
      if (hall[h]) break;
      if (hallSpaces.has(h)) legalMoves.push({ kind: "hall", slot: h });
      if (h === goalRoom.slot && goalRoomValid) {
        let destSpace = goalRoom.spaces[1] ? 0 : 1;
        return [{ kind: "room", index: goalRoomIdx, space: destSpace }];
      }
    }

    return legalMoves;
  }
  const mob = hall[pos.slot];
  if (!mob) return [];
  const i = "ABCD".indexOf(mob);
  let goalRoom = rooms[i];
  for (
    let h = pos.slot + Math.sign(goalRoom.slot - pos.slot);
    h !== goalRoom.slot;
    h += Math.sign(goalRoom.slot - pos.slot)
  ) {
    if (hall[h]) return [];
  }
  if (goalRoom.spaces.find((other) => other && other !== mob)) {
    return [];
  }
  let destSpace = goalRoom.spaces[1] ? 0 : 1;
  return [{ kind: "room", index: "ABCD".indexOf(mob), space: destSpace }];
}

type Move = [Pos, Pos];
function* allLegalMoves(state: State): Generator<Move> {
  for (const slot of Array.from(hallSpaces)) {
    let currentPos: Pos = { kind: "hall", slot };
    for (const move of legalMoves(currentPos, state)) {
      yield [currentPos, move];
    }
  }

  for (let i = 0; i < 4; i++) {
    for (const space of [0, 1]) {
      let currentPos: Pos = { kind: "room", index: i, space };
      for (const move of legalMoves(currentPos, state)) {
        yield [currentPos, move];
      }
    }
  }
}

function isSolved([hall, rooms]: State) {
  if (hall.some((x) => x)) {
    return false;
  }
  return rooms.every((r, i) => {
    return r.spaces.every((s) => s === "ABCD"[i]);
  });
}

const mobEnergy = (mob: string): number => Math.pow(10, "ABCD".indexOf(mob));

function energyCost([from, to]: Move, state: State) {
  if (from.kind === "room") {
    const mob = state[1][from.index].spaces[from.space];
    return mobEnergy(mob!) * dist(from, to as HallPos);
  }
  const mob = state[0][from.slot];
  return mobEnergy(mob!) * dist(to as RoomPos, from);
}
function dist(from: RoomPos, to: HallPos | RoomPos) {
  let distOutOfRoom = from.space + 1;
  let destSlot = to.kind === "hall" ? to.slot : slotForRoomIdx(to.index);
  const hallDist = Math.abs(slotForRoomIdx(from.index) - destSlot);
  let distIntoRoom = to.kind === "room" ? to.space + 1 : 0;
  return distOutOfRoom + hallDist + distIntoRoom;
}

const getPos = (pos: Pos, state: State) => {
  if (pos.kind === "room") {
    return state[1][pos.index].spaces[pos.space];
  }
  return state[0][pos.slot];
};
const setPos = (pos: Pos, mob: Mob | null, state: State) => {
  if (pos.kind === "room") {
    state[1][pos.index].spaces[pos.space] = mob;
    return;
  }
  state[0][pos.slot] = mob;
};

function makeMove(state: State, [from, to]: Move) {
  const newState = JSON.parse(JSON.stringify(state));
  setPos(to, getPos(from, newState), newState);
  setPos(from, null, newState);
  return newState;
}
let bestSolution = Infinity;
function solve(state: State, energy: number): number {
  if (energy >= bestSolution) return Infinity;
  if (isSolved(state)) {
    console.log(`Found solution ${energy}`);
    bestSolution = energy;
    return energy;
  }
  const solutions = Array.from(allLegalMoves(state)).map((move) =>
    solve(makeMove(state, move), energy + energyCost(move, state))
  );
  if (solutions.length === 0) return Infinity;
  return Math.min(...solutions);
}

const state = [hallways, rooms] as const;
// console.log(legalMoves({ kind: "room", index: 0, space: 0 }, state));
// console.log(
//   dist({ kind: "room", index: 0, space: 0 }, { kind: "hall", slot: 0 })
// );

console.log(Array.from(allLegalMoves(state)));
console.log(solve(state, 0));
