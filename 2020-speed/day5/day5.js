const _ = require("lodash");
const fs = require("fs");

/*
const file = "example.txt";
/*/
const file = "input.txt";
// */

const text = fs.readFileSync(file).toString().trim();

const rows = text.split("\n").map(line => {
    return parseInt(line.replace(/(B|R)/g,1).replace(/(F|L)/g, 0), 2);
});

const r = _.sortBy(rows);
console.log(JSON.stringify(r));
let i = r.shift();
console.log(i);
for(const row of r) {
    if(row !== i + 1) {
        console.log(row)
        break;
    }
    i = row;
}

console.log(r[r.length - 1]);
