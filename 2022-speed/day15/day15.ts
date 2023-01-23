import fs, { chownSync } from "fs";
import _ from "lodash";
//*
const path = "input.txt";
const by = 2000000;
/*/
const path = "example.txt";
const by = 10;
//*/

const inputFile = fs.readFileSync(path).toString().trim();
const lines = inputFile.split("\n");

const data = lines.map((line) => line.match(/\d+/g)!.map((x) => parseInt(x)));

let minX = Infinity;
let maxX = -Infinity;
let minY = Infinity;
let maxY = -Infinity;

const dist = (sx: number, sy: number, bx: number, by: number) =>
  Math.abs(bx - sx) + Math.abs(by - sy);

const beaconSet = new Set<string>();
const sensorSet = new Set<string>();
const asStr = (x: number, y: number) => `${x},${y}`;

const sensors = data.map(([sx, sy, bx, by]) => {
  const range = dist(sx, sy, bx, by);
  minX = Math.min(sx - range - 1, minX);
  maxX = Math.max(sx + range + 1, maxX);
  minY = Math.min(sy - range - 1, minY);
  maxY = Math.max(sy + range + 1, maxY);
  beaconSet.add(asStr(bx, by));
  sensorSet.add(asStr(sx, sy));
  return { sx, sy, range };
});

console.log(sensors, minX, maxX);

// let i = 0;
// for (let bx = minX - 5; bx <= maxX + 5; bx++) {
//   for (let { sx, sy, range } of sensors) {
//     if (dist(sx, sy, bx, by) <= range) {
//       //   console.log({ bx, by, sx, sy }, dist(sx, sy, bx, by));
//       if (!beaconSet.has(`${bx},${by}`)) i++;
//       break;
//     }
//   }
// }
// console.log(i - 1);
let i = 0;
// console.log(
//   _.range(minY, maxY)
//     .map(
//       (y) =>
//         y.toString().padStart(4) +
//         _.range(minX, maxX)
//           .map((x) => {
//             if (beaconSet.has(asStr(x, y))) return "B";
//             if (sensorSet.has(asStr(x, y))) return "S";
//             for (let { sx, sy, range } of sensors) {
//               if (dist(sx, sy, x, y) <= range) {
//                 if (y === by) i++;
//                 return "#";
//               }
//             }
//             return " ";
//           })
//           .join("")
//     )
//     .join("\n")
// );

for (let x = minX; x <= maxX; x++) {
  let y = by;
  if (beaconSet.has(asStr(x, y))) continue;
  if (sensorSet.has(asStr(x, y))) continue;
  for (let { sx, sy, range } of sensors) {
    if (dist(sx, sy, x, y) <= range) {
      i++;
      break;
    }
  }
}
console.log(i);
