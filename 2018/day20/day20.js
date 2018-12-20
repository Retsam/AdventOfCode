const _ = require("lodash");
const fs = require("fs");

const input = fs.readFileSync("input.txt").toString().trim();


// const input = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$";
// const input = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
// const input = "^ENWWW(NEEE|SSE(EE|N))$"
const regex = input.slice(1, input.length-1);
console.log(regex);

let connectionsMap = new Map();
let branchStack = [];

let pos = [0,0];
for(const char of regex) {
    if(char === "(") {
        branchStack.push(pos);
        continue;
    }
    if(char === ")") {
        branchStack.pop();
        continue;
    }
    if(char === "|") {
        pos = branchStack[branchStack.length - 1];
        continue;
    }
    const [x,y] = pos;
    const newPos = (
        char === "E" ? [x+1, y] :
        char === "N" ? [x, y+1] :
        char === "W" ? [x-1, y] :
        char === "S" ? [x, y-1] :
        undefined
    );
    if(!newPos) throw new Error("NOPE")
    // console.log(`${pos} connects to ${newPos}`)
    const posConnections = connectionsMap.get(pos.toString()) || [];
    const newPosConnections = connectionsMap.get(newPos.toString()) || [];
    posConnections.push(newPos.toString());
    newPosConnections.push(pos.toString());
    connectionsMap.set(pos.toString(), posConnections)
    connectionsMap.set(newPos.toString(), newPosConnections);
    pos = newPos;
}

pos = [0,0];

let searching = [[pos.toString(), 0]];
let toSearch = new Set(Array.from(connectionsMap.keys()));
toSearch.delete(pos.toString());
let found = [];

while(true) {
    let [current, dist] = searching.pop();
    // console.log(current);
    // console.log(connectionsMap.get(current));
    let newConnections = _.uniq(connectionsMap.get(current))
        .filter(c => toSearch.has(c));

    newConnections.forEach(c => {
        // console.log(`Found ${c} at distance ${dist}`);
        toSearch.delete(c)
        found.push([c, dist+1])
    });
    if(!Array.from(toSearch.keys()).length) {
        // console.log(newConnections);
        // console.log(dist);
        break;
    }
    searching = searching.concat(newConnections.map(c => [c, dist+1]));
    // searching = newConnections.map(c => [c, dist+1]).concat(searching);

}
// console.log(found);
console.log(_.maxBy(found, ([x,dist]) => dist))
const far = _.filter(found, ([x, dist]) => dist >= 1000);
// console.log(far);
console.log(far.length);
