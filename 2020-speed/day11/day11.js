const fs = require("fs");
/*
const file = "example.txt";
/*/
const file = "input.txt";
// */

const input = fs.readFileSync(file, "utf-8").trim();
const lines = input.split("\n");

function adjacent(x, y) {
    return [[x - 1, y], [x + 1, y], [x-1, y-1], [x-1, y+1], [x+1,y-1],[x+1,y+1],[x,y-1],[x,y+1]]
}
// function step(lines) {
//     return lines.map((line, x) => line.split("").map((seat, y) => {
//         if(seat === ".") return ".";
//         if(seat === "L") {
//             if(!adjacent(x,y).some(([x,y]) => (lines[x] || [])[y] === "#")) {
//                 return "#";
//             }
//             return "L"
//         }
//         if(adjacent(x,y).filter(([x,y]) => (lines[x] || [])[y] === "#").length >= 4) {
//             return "L"
//         }
//         return "#"
//     }).join(""))
// }

function checkDir(from, dx, dy, grid) {
    let [x, y] = from;
    let square;
    do {
        x += dx;
        y += dy;
        square = (grid[x] || [])[y]
        if(square === "L") return false;
        if(square === "#") return true;
    } while(square);
    return false;
}

const dirs = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1,1]];

function step(lines) {
    return lines.map((line, x) => line.split("").map((seat, y) => {
        if(seat === ".") return ".";
        if(seat === "L") {
            if(dirs.filter(([dx, dy]) =>
                checkDir([x, y], dx, dy, lines)
            ).length === 0) {
                return "#";
            }
            return "L"
        }
        if(dirs.filter(([dx, dy]) =>
            checkDir([x, y], dx, dy, lines)
        ).length > 4) {
            return "L"
        }
        return "#"
    }).join(""))
}

let seats = lines;
let text;
while(seats.join("") !== text) {
    text = seats.join("")
    seats = step(seats);
    console.log(seats.join("\n"), "\n\n");
}

console.log(seats.join("").split("").filter(x => x === "#").length)
