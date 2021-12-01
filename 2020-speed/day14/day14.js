const fs = require("fs");
const _ = require( "lodash")
//*
const file = "example.txt";
/*/
const file = "input.txt";
// */

const input = fs.readFileSync(file, "utf-8").trim();
const lines= input.split("\n");

// const [mask, ...commands] = lines;

let memory = new Map();
let mask;
lines.forEach(cmd => {
    if(cmd.startsWith("mask = ")) {
        mask = cmd.slice(7)
        // onMask = parseInt(cmd.slice(7).replace(/X/g,0), 2)
        // offMask = parseInt(cmd.slice(7).replace(/1/g, "X").replace(/0/g, "1").replace(/X/g,0), 2)
        return;
    }
    const [reg, val] = cmd.match(/(\d+)/g);
    let masked = parseInt(_.padStart(parseInt(val).toString(2), 36, "0").split("").map((c, i) => {
        // console.log(mask[i])
        return mask[i] === "X" ? c : mask[i];
    }).join(""), 2);
    memory.set(reg, masked);
})

console.log(Array.from(memory.values()).reduce((a, b) => a + b));
