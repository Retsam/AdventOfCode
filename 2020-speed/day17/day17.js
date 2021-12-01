const fs = require("fs");
const { isContext } = require("vm");
/*
const file = "example.txt";
/*/
const file = "input.txt";
// */

const input = fs.readFileSync(file, "utf-8").trim();
const lines = input.split("\n");

let minZ = 0;
let maxZ = 0;
let minX = 0;
let maxX = lines[0].length;
let minY = 0;
let maxY = lines.length;

let coord = (x, y, z) => `${x},${y},${z}`;

let state = new Set();
lines.forEach((line, y) => {

    line.split("").forEach((char, x) => {
        if(char === "#") {
            state.add(coord(x, y, 0));
        }
    })
})

const n = [-1, 0, 1];
const neighbors = n.flatMap(x =>
    n.flatMap(y =>
        n.flatMap(z => {
            if(x === 0 && y === 0 && z === 0) return [];
            return [[x,y,z]]
        })
    )
)
function iter(prevState) {
    minX -= 1;
    maxX += 1;
    minZ -= 1;
    maxZ += 1;
    minY -= 1;
    maxY += 1;
    let state = new Set();
    for(let x=minX;x<maxX;x++) {
        for(let y=minY;y<maxY;y++) {
            for(let z=minZ;z<maxZ;z++) {
                let activeNeighbors = neighbors.filter(([dx, dy, dz]) => {
                    return prevState.has(coord(dx + x, dy + y, dz + z));
                }).length;
                let isActive = prevState.has(coord(x, y, z)) ?
                    (activeNeighbors === 2 || activeNeighbors === 3) :
                    (activeNeighbors === 3);
                if(isActive) {
                    state.add(coord(x,y,z));
                }
            }
        }
    }
    return state;
}

for(let i=0;i<6;i++) {
    state = iter(state);
    console.log(state.size);
}

