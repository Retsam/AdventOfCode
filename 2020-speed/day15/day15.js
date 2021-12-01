const fs = require("fs");
//*
const file = "example.txt";
/*/
const file = "input.txt";
// */

const input = fs.readFileSync(file, "utf-8").trim();
const nums = input.split(",");

let i = 0;
let numsAge = new Map();
nums.slice(0, nums.length - 1).forEach(n => {
    numsAge.set(parseInt(n), i)
    i++;
})

let prevNum = parseInt(nums.pop());

while(i < (30000000 - 1)) {
    const lastSpoke = numsAge.get(prevNum);
    const nextNum = lastSpoke === undefined ? 0 : i - lastSpoke;
    numsAge.set(prevNum, i);
    i++;
    prevNum = nextNum;
}
console.log(prevNum)
