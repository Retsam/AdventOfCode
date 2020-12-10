const fs = require("fs");
const _ = require("lodash");
/*
const file = "example.txt";
/*/
const file = "input.txt";
// */

const input = fs.readFileSync(file, "utf-8").trim();
const lines = _.sortBy(input.split("\n").map(x => parseInt(x)));

lines[lines.length] = lines[lines.length - 1] + 3

// Part 1
let j = 0;
let c1 = 0;
let c3 = 0;
let n;

let adapters = lines.slice();
while(n = adapters.shift()) {
    if(n - j === 1) {
        c1++;
    }
    if(n - j === 3) {
        c3++;
    }
    if(n - j > 3) {
        break;
    }
    j = n;
}
console.log(c1 * c3);

// Part 2

const calcOptions = _.memoize((voltage, adapters) => {
    if(adapters.length === 0) {return 1;}
    let options = 0;
    if(adapters[0] <= voltage + 3) {
        options += calcOptions(adapters[0], adapters.slice(1))
    }
    if(adapters[1] <= voltage + 3) {
        options += calcOptions(adapters[1], adapters.slice(2))
    }
    if(adapters[2] <= voltage + 3) {
        options += calcOptions(adapters[2], adapters.slice(3))
    }
    return options
})
console.log(calcOptions(0, lines));


