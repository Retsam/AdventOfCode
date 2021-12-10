const fs = require("fs");
const nodePath = require("path");
const _ = require("lodash");
//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
const input = fs
  .readFileSync(nodePath.join(__dirname, path))
  .toString()
  .trim()
  .split("\n");

let errScore = 0;

const acScores = input
  .map((line) => {
    const stack = [];
    for (const char of line.split("")) {
      if ("([<{".includes(char)) {
        stack.push(char);
      } else {
        const actual = stack.pop();

        const expected = {
          "(": ")",
          "[": "]",
          "<": ">",
          "{": "}",
        }[actual];
        if (!actual || char !== expected) {
          const err = {
            ")": 3,
            "]": 57,
            "}": 1197,
            ">": 25137,
          }[char];
          errScore += err;
          return;
        }
      }
    }
    let score = 0;
    let next;
    for (const next of stack.reverse()) {
      score *= 5;
      score += {
        "(": 1,
        "[": 2,
        "{": 3,
        "<": 4,
      }[next];
    }
    return score;
  })
  .filter((x) => x)
  .sort((a, b) => b - a);
const acWinner = acScores[(acScores.length - 1) / 2];

console.log(errScore, acWinner);
