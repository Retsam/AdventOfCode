const fs = require("fs");
const nodePath = require("path");
const _ = require("lodash");

const path = "input.txt";
let input = fs.readFileSync(nodePath.join(__dirname, path)).toString().trim();

const sum = (xs) => xs.reduce((a, b) => a + b);
const prod = (xs) => xs.reduce((a, b) => a * b);

const fromHex = (x) => parseInt(x, 16);
const fromBin = (x) => parseInt(x, 2);
const hexToBin = (x) =>
  x
    .split("")
    .map((x) => fromHex(x, 16).toString(2).padStart(4, "0"))
    .join("");

const parse = (ph, part1) => {
  let binStr = hexToBin(ph);

  // Keep the pointer of our current position in the string
  let c = 0;
  // Read n bits from the string, based on the current position
  function read(n) {
    c += n;
    return binStr.slice(c - n, c);
  }
  // Get the current position
  const pos = () => c;
  return parsePacket(read, pos, part1);
};

const parsePacket = (read, pos, part1) => {
  const id = fromBin(read(3));
  const type = fromBin(read(3));

  if (type === 4) {
    let seg,
      bin = "";
    do {
      seg = read(5);
      bin += seg.slice(1);
    } while (seg[0] === "1");
    return part1 ? id : fromBin(bin);
  }

  const lenType = read(1);
  let subPackets = [];
  if (lenType === "0") {
    let bitLen = fromBin(read(15));
    let iPos = pos();
    while (pos() < iPos + bitLen) {
      subPackets.push(parsePacket(read, pos, part1));
    }
  } else {
    let packetCount = fromBin(read(11));
    for (let i = 0; i < packetCount; i++) {
      subPackets.push(parsePacket(read, pos, part1));
    }
  }
  if (part1) {
    return id + sum(subPackets);
  }

  const cmp = (cond) => () => cond(subPackets[0], subPackets[1]) ? 1 : 0;
  return {
    0: () => sum(subPackets),
    1: () => prod(subPackets),
    2: () => Math.min(...subPackets),
    3: () => Math.max(...subPackets),
    5: cmp((a, b) => a > b),
    6: cmp((a, b) => a < b),
    7: cmp((a, b) => a === b),
  }[type]();
};

// console.log(parse("8A004A801A8002F478", true)); // version sum 16
// console.log(parse("04005AC33890")); // 5 * 9 == 54

console.log(parse(input, true));
console.log(parse(input));
