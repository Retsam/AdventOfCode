const fs = require("fs");
const _ = require( "lodash")
/*
const file = "example.txt";
/*/
const file = "input.txt";
// */

const input = fs.readFileSync(file, "utf-8").trim();
const [_t, _buses] = input.split("\n");

// Part 1
// const t = parseInt(_t);
// const buses = _buses.split(",").filter(b => b !== "x").map(x => parseInt(x));

// console.log(_.sortBy(buses.map(b =>
//     [b, b * (Math.floor(t / b) + 1) - t]
// ), ([x,y]) => y))

const offsets = _buses.split(",").map((b, i) => [parseInt(b), i]).filter(([b]) => !isNaN(b))
let [countBy, off] = offsets.shift();
let next;
// Each loop adds the next bus into the solution
while(next = offsets.shift()) {
    let [m, i] = next;
    // Keep counting until we find a solution that satisfies all
    //  the busses that we're currently considering
    while((off + i) % m !== 0) {
        off += countBy;
    }
    // in the first loop, count by `a`,
    // in the second loop, count by `a * b`, etc
    countBy = m * n;
}

console.log(off);
