const fs = require("fs");
const nodePath = require("path");

/*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
const input = fs
  .readFileSync(nodePath.join(__dirname, path))
  .toString()
  .trim()
  .split("\n");

const numbers = input[0].split(",").map((x) => +x);
let boards = input
  .slice(2)
  .join("\n")
  .split("\n\n")
  .map((board) =>
    board.split("\n").map((line) =>
      line
        .trim()
        .split(/\s+/)
        .map((x) => +x)
    )
  );

function wins(board, numSet) {
  for (let i = 0; i < 5; i++) {
    let winV = true;
    let winH = true;
    for (let j = 0; j < 5; j++) {
      winV = winV && numSet.has(board[i][j]);
      winH = winH && numSet.has(board[j][i]);
    }
    if (winV || winH) return true;
  }
  return false;
}

function score(board, drawn) {
  const sum = board
    .flat()
    .filter((x) => !drawn.has(x))
    .reduce((a, b) => a + b);
  const lastDraw = Array.from(drawn)[drawn.size - 1];
  return sum * lastDraw;
}

let drawn = new Set([]);
let nextNum;
let loser;
let winner;
while (boards.length) {
  nextNum = numbers.shift();
  drawn.add(nextNum);
  if (!winner) {
    winner = boards.find((b) => wins(b, drawn));
    if (winner) {
      console.log(score(winner, drawn));
    }
  }
  boards = boards.filter((b) => !wins(b, drawn));
  if (boards.length === 1) {
    loser = boards[0];
  }
}

console.log(score(loser, drawn));
