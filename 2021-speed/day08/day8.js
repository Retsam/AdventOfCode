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
    //  d is the only segment in four that's in 7 digits
    const fourPattern = inp.find((x) => x.length === 4).split("");
    map.d = fourPattern.find((c) => countSegment(c) === 7);

    // e is the only segment that only appears 4 times
    map.e = "abcdefg".split("").find((c) => countSegment(c) === 4);

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
