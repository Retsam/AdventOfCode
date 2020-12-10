const _ = require("lodash");
const fs = require("fs");

/*
const file = "example.txt";
/*/
const file = "input.txt";
// */

const input = fs.readFileSync(file).toString().trim();

const part1 = input.split("\n\n")
    .map(group => _.uniq(group.split("\n").join("")).length)
    .reduce((a, b) => a + b);

console.log(part1);
// const groups = input.split("\n\n");
// // const counts = groups.map(group =>
// //     _.uniq(group.split("\n").join("")).length
// // );
// const counts = groups.map(group => {
//     const allLetters = _.uniq(group.split("\n").join(""));
//     return allLetters.filter(l =>
//         group.split("\n").every(p => p.includes(l))
//     ).length;

// });
// console.log(counts);
// console.log(counts.reduce((a, b) => a + b));
