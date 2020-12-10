const fs = require("fs");

/*
const file = "example.txt";
/*/
const file = "input.txt";
// */

const input = fs.readFileSync(file, "utf-8").trim();
const lines = input.split("\n").map(line => line.split(" "))

function executeProg(lines) {
    let acc = 0;
    let i = 0;
    let visited = new Set();
    while(!visited.has(i)) {
        visited.add(i);
        if(i === lines.length) {
            return ["terminates", acc]
        }
        const line = lines[i];
        if(!line) { return [ "errors" ] }
        const [cmd, val] = line;
        if(cmd === "acc") {
            acc += (+val);
        }
        if(cmd === "jmp") {
            i += (+val);
        } else {
            i += 1;
        }
    }
    return ["loops", acc];
}

console.log(executeProg(lines));

for(const i in lines) {
    const [cmd, value] = lines[i];
    if(cmd === "acc") continue;

    const newProg = lines.slice();
    newProg[i] = [cmd === "nop" ? "jmp" : "nop", value]
    const res = executeProg(newProg);
    if(res[0] === "terminates") {
        console.log(res);
        break;
    }
}
