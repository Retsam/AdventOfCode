const fs = require("fs");
const _ = require("lodash");
/*
const file = "example.txt";
/*/
const file = "input.txt";
// */

const input = fs.readFileSync(file, "utf-8").trim();

const lines = input.split("\n")

let [x, y, dir] = [0, 0, "E"];
lines.forEach(line => {
    let [C, V] = [line[0], parseInt(line.slice(1))]
    if(C === "F") C = dir;
    if(C === "N") y += V;
    if(C === "S") y -= V;
    if(C === "E") x += V;
    if(C === "W") x -= V;
    if(C === "R") {
        dir = "NESW"[("NESW".indexOf(dir) + V / 90) % 4]
    }
    if(C === "L") {
        dir = "NWSE"[("NWSE".indexOf(dir) + V / 90) % 4]
    }
})

let x = 0;
let y = 0;
let wx = 10;
let wy = 1;

lines.forEach(line => {
    let C = line[0];
    const V = parseInt(line.slice(1));
    if(C === "F") {
        x += wx * V;
        y += wy * V;
    }
    if(C === "N") wy += V;
    if(C === "S") wy -= V;
    if(C === "E") wx += V;
    if(C === "W") wx -= V;
    if(C === "R" || C === "L") {
        let v = C === "R" ? V / 90 : (-V / 90 + 4) % 4;
        for(let vv=0; vv<v; vv++) {
            [wx, wy] = [wy, -wx];
        }
    }
})
console.log(Math.abs(x)+  Math.abs(y))
