const _ = require("lodash");

// Rack ID = X + 10
// Power level: rack id * y
// Power Level += input
// powerlevel *= rackid
// keep 100s digit
// subtract 5

// const serialId = 18;
const serialId = 4151;

const getPower = _.memoize((x, y) => {
    if(x > 300 || y > 300) throw new Error("oops" + x +", "+  y)
    const rackId = x + 10;
    let powerLevel = (rackId * y) + serialId;
    powerLevel *= rackId;
    return parseInt(powerLevel.toString().slice(-3, -2)) - 5;
}, (x, y) => `${x},${y}`);

let best = -1000000
let bestCoords = null;

const checkBest = (coords, level) => {
    if(level > best) {
        best = level;
        bestCoords = coords;
    }
}

for(const x of _.range(1, 301)) {
    console.log(`Trying ${x}`)

    for(const y of _.range(1, 301)) {

        const maxZ = Math.min(301-x, 301-y);
        let level = 0;
        for(const z of _.range(1, maxZ+1)) {
            for(const zz of _.range(0, z)) {
                level += getPower(x+zz, y+z-1)

                level += getPower(x+z-1, y+zz)
            }
            level -= getPower(x+maxZ-1, y+maxZ-1);
            checkBest([x, y, z], level);
        }
    }
}

console.log(bestCoords)
