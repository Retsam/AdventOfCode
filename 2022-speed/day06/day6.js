const fs = require("fs");

//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/

const input = fs.readFileSync(path).toString();

const findMessageIndex = (size) =>
  input
    .split("")
    .findIndex((_, i) => new Set(input.slice(i - size, i)).size === size);

console.log(findMessageIndex(4));
console.log(findMessageIndex(14));
