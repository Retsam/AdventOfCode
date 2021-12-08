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

const data = input.map((str) => str.split(" | ").map((l) => l.split(" ")));

let p1 = 0;

const p2 = _.sum(
  data.map(([inp, out]) => {
    // mapping from normal segment to the corresponding segment in the input, we need c,d,e
    let map = {};

    // Count how many numbers a segment is included in
    const countSegment = (c) =>
      _.filter(inp.join("").split(""), (x) => x === c).length;

    // Identify which 1 segment is c, based on usage
    // (The f segment is in 9 digits, the c segment is only in 8)
    const onePattern = inp.find((x) => x.length === 2).split("");
    map.c = countSegment(onePattern[0]) === 8 ? onePattern[0] : onePattern[1];

    // Use the four pattern (known by length) to identify d
    const uniqueToFour = _.difference(
      inp.find((x) => x.length === 4).split(""),
      onePattern
    );
    // either b or d, but only d is used 7 times
    map.d =
      countSegment(uniqueToFour[0]) === 7 ? uniqueToFour[0] : uniqueToFour[1];

    // Use the nine pattern to identify e (the only segment it doesn't have)
    const ninePattern = inp
      .find((x) => x.length === 6 && x.includes(map.d) && x.includes(map.c))
      .split("");
    map.e = _.difference("abcdefg".split(""), ninePattern)[0];

    const getNumber = (pattern) => {
      if (pattern.length === 5) {
        if (pattern.includes(map.e)) return "2";
        if (pattern.includes(map.c)) return "3";
        return "5";
      }
      if (pattern.length === 6) {
        if (!pattern.includes(map.d)) return "0";
        if (pattern.includes(map.e)) return "6";
        return "9";
      }
      p1 += 1;
      if (pattern.length === 2) return "1";
      if (pattern.length === 3) return "7";
      if (pattern.length === 4) return "4";
      if (pattern.length === 7) return "8";
    };

    return +out.map(getNumber).join("");
  })
);

console.log(p1, p2);
