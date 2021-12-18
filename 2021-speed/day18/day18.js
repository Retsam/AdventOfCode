const fs = require("fs");
const nodePath = require("path");
const _ = require("lodash");
/*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
let lines = fs
  .readFileSync(nodePath.join(__dirname, path))
  .toString()
  .trim()
  .split("\n")
  .map((d) => JSON.parse(d));

/* Uses a concept of "addresses" (more formally "lenses", I think)
   an address is a list of indexes that leads a particular item, with utilities for getting/setting
   the value at a particular address.  Makes finding "adjacent" elements easy */

// Converts [[a, b], c] into [[0,0], [0,1], [1]]
function buildAddresses(data, path = []) {
  return data.flatMap((d, i) => {
    const newPath = path.concat(i);
    return typeof d === "number" ? [newPath] : buildAddresses(d, newPath);
  });
}

// Gets the value in data at the provided address
const atAddress = (data, address) => {
  for (const i of address) data = data[i];
  return data;
};

const parentAddr = (addr) => addr.slice(0, addr.length - 1);

// Updates the value in data at the address
const setAddress = (data, address, updateFn) => {
  const parent = atAddress(data, parentAddr(address));
  const indexInParent = address[address.length - 1];
  parent[indexInParent] = updateFn(parent[indexInParent]);
};

// Returns truthy if it does an explode (returns the data for easy logging purposes)
const findExplode = (data) => {
  const addresses = buildAddresses(data);
  // An address of length five means that it's a number inside four tuples, so that's the tuple to explode
  const explodeAt = _.findIndex(addresses, { length: 5 });
  if (explodeAt === -1) return false;

  const tupleAddr = parentAddr(addresses[explodeAt]);
  const [l, r] = atAddress(data, tupleAddr);

  // Find 'adjacent' numbers by looking at adjacent elements in the addresses array
  // Update the element to the left, if there is one
  const prevAddr = addresses[explodeAt - 1];
  if (prevAddr) setAddress(data, prevAddr, (x) => x + l);
  // Update the element to the right, if there is one
  // (+2, because +1 will be the right half of the same exploding pair)
  const nextAddr = addresses[explodeAt + 2];
  if (nextAddr) setAddress(data, nextAddr, (x) => x + r);

  // Replace the tuple with 0
  setAddress(data, tupleAddr, () => 0);
  return data;
};

// console.log(JSON.stringify(findExplode([[[[[9, 8], 1], 2], 3], 4])));
// console.log(JSON.stringify(findExplode([7, [6, [5, [4, [3, 2]]]]])));
// console.log(JSON.stringify(findExplode([[6, [5, [4, [3, 2]]]], 1])));
// console.log(JSON.stringify(findExplode([[3, [2, [1, [7, 3]]]],[6, [5, [4, [3, 2]]]]])));
// console.log(JSON.stringify(findExplode([[3, [2, [8, 0]]],[9, [5, [4, [3, 2]]]]])));

// Returns truthy if it finds a split (returns the data for easy logging purposes)
const findSplit = (data) => {
  const addresses = buildAddresses(data);
  const splitAddr = addresses.find((addr) => atAddress(data, addr) > 9);
  if (!splitAddr) return false;
  setAddress(data, splitAddr, (val) => [
    Math.floor(val / 2),
    Math.ceil(val / 2),
  ]);
  return data;
};

const process = (_data) => {
  const data = _.cloneDeep(_data);
  while (findExplode(data) || findSplit(data)) {
    /* side effects are awesome */
  }
  return data;
};

const magnitude = (data) => {
  if (typeof data === "number") return data;
  return 3 * magnitude(data[0]) + 2 * magnitude(data[1]);
};

console.log(magnitude(lines.reduce((a, b) => process([a, b]))));

const pairs = lines
  .flatMap((l1) => lines.map((l2) => [l1, l2]))
  .filter(([a, b]) => a !== b);

console.log(_.max(pairs.map(process).map(magnitude)));
