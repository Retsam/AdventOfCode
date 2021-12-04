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

const len = input[0].length;
const count = input.length;

const multiplyBinary = (a, b) => parseInt(a, 2) * parseInt(b, 2);

let gamma = "";
let epsilon = "";
for (let i = 0; i < len; i++) {
  let c = 0;
  for (let data of input) {
    if (data[i] === "1") c++;
  }
  if (c > count / 2) {
    gamma += "1";
    epsilon += "0";
  } else {
    gamma += "0";
    epsilon += "1";
  }
}

console.log(multiplyBinary(gamma, epsilon));

function countBits(arr, i) {
  let onesCount = arr.filter((c) => c[i] === "1").length;
  return [arr.length - onesCount, onesCount];
}

function calculateRating(criteria) {
  let arr = input;
  let i = 0;
  while (arr.length > 1) {
    const [zeros, ones] = countBits(arr, i);
    let lookingFor = criteria(zeros, ones);
    arr = arr.filter((str) => str[i] === lookingFor);
    i++;
  }
  return arr[0];
}

const ogRating = calculateRating((zeros, ones) => (zeros > ones ? "0" : "1"));
const co2Rating = calculateRating((zeros, ones) => (zeros <= ones ? "0" : "1"));
console.log(multiplyBinary(ogRating, co2Rating));
