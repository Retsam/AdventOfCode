const fs = require("fs");
/*
const file = "example.txt";
/*/
const file = "input.txt";
// */

const input = fs.readFileSync(file, "utf-8").trim();
const [rules, myticket, nearby] = input.split("\n\n").map(l => l.split("\n"));

const data = rules.map(rule => {
    let [field, ranges] = rule.split(": ");

    return [
        field,
        ranges.split(" or ").map(range => range.split("-").map(x => parseInt(x))),
        new Set(rules.map((r, i) => i))
    ];
})

let invalid = 0;
function matchesRange(d, [min, max]) {
    return d >= min && d <= max;
}
myticket.slice(1).concat(nearby.slice(1)).forEach((ticket, i) => {
    const values = ticket.split(",").map(x => parseInt(x));
    if(values.some(d => !data.some(
        ([, [r1, r2]]) => matchesRange(d, r1) || matchesRange(d, r2)
    ))) {
        return; // invalid
    }
    values.forEach((d, i) => {
        data.forEach(([, [r1, r2], set]) => {
            if(!(matchesRange(d, r1) || matchesRange(d, r2))) {
                set.delete(i)
            }
        })
    });
});

console.log(data.slice(0, 6).map(([rule, , set]) => [rule, Array.from(set)]))

let changed = false;
let mapping = new Map();
let claimed = new Set();

do {
    changed = false;
    data.forEach(([field, , set]) => {
        if(set.size === 0) return;
        for(const i of claimed) {
            if(set.has(i)) {
                changed = true;
                set.delete(i);
            }
        }
        if(set.size === 1) {
            let i = Array.from(set)[0];
            mapping.set(field, i);
            claimed.add(i);
            changed = true;
            set.delete(i);
        }
    })
} while(changed === true);

let product = 1;
let myTicket = myticket[1].split(",").map(x => parseInt(x));
for(const [key, val] of mapping) {
    if(key.startsWith("departure")) {
        product *= myTicket[val];
        console.log([key, val, myTicket[val]]);
    }
}
console.log(product)
