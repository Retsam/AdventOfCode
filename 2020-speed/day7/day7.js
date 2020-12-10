const _ = require("lodash");
const fs = require("fs");

/*
const file = "example.txt";
/*/
const file = "input.txt";
// */

const input = fs.readFileSync(file).toString().trim();

let contentsMap = new Map();

input.split("\n").forEach(line => {
    const [type, rest] = line.split(" bags contain ");
    const contents = rest.includes("no other") ? [] : rest.slice(rest, rest.length-1).split(", ").map(content => {
        const words = content.split(" ");
        const number = parseInt(words.shift());
        words.pop();
        return [number, words.join(" ")];
    })
    contentsMap.set(type, contents);
});

function containsGold(type) {
    const contents = contentsMap.get(type);
    for(const [number, kind] of contents) {
        if(kind === "shiny gold") {
            return true;
        }
        if(containsGold(kind)) {
            return true;
        }
    }
    return false;
}

let goldCount = 0;
for(const [type] of contentsMap) {
    if(containsGold(type)) {
        goldCount++;
    }
}

console.log(goldCount);

function contentsCount(type) {
    const contents = contentsMap.get(type);
    let c = 0;
    for(const [number, kind] of contents) {
        c += contentsCount(kind) * number;
    }
    console.log(type)
    console.log(c);
    return c + 1;

}
console.log(contentsCount("shiny gold"));
// let goldContents = 0;
// console.log(JSON.stringify(data));
