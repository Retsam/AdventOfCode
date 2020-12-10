const fs = require("fs");
const _ = require("lodash");
/*
const file = "example.txt";
/*/
const file = "input.txt";
// */

const input = fs.readFileSync(file, "utf-8").trim();
const lines = input.split("\n").map(x => parseInt(x));




// const preamble = lines.slice(0, 25);
// const rest = lines.slice(25);

// let n = rest.shift();
// while(n) {
//     let found = false;
//     for(const x of preamble) {
//         for(const y of preamble) {
//             if(x + y === n) {
//                 found = true;
//             }
//         }
//     }
//     if(!found) {
//         console.log(n);
//         break;
//     }

//     preamble.shift();
//     preamble.push(n);
//     n = rest.shift();
// }

const goal = 393911906;
// const goal = 127;

let lists = [];
let n = lines.shift();
const sum = (list) => list.reduce((a, b) => a + b);
while(n !== undefined) {
    lists = lists.map(l => l.concat(n));
    lists.push([n]);

    while(sum(lists[0]) >= goal) {
        const list = lists.shift();
        if(sum(list) === goal) {
            let min = list[0];
            let max = list[0];

            console.log(list);
            console.log(_.min(list) + _.max(list));
            throw new Error("done")
        }
    }
    n = lines.shift();
}
