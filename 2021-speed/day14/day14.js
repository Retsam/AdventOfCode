const fs = require("fs");
const nodePath = require("path");
const _ = require("lodash");
/*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
let input = fs
  .readFileSync(nodePath.join(__dirname, path))
  .toString()
  .trim()
  .split("\n");

let template = input[0];
const rules = input.slice(2).map((r) => r.split(" -> "));

// Convert "ABCB" into {A: 1, B: 2, C: 1}
const getCounts = (str) => {
  return str.split("").reduce((cts, c) => {
    cts[c] = (cts[c] ?? 0) + 1;
    return cts;
  }, {});
};

// Combine {A: 1, B: 1} and {B: 1, C: 1} into {A: 1, B: 2, C: 1}
const combineCounts = (cts) =>
  cts.reduce((counts, newCounts) => {
    for (const c of Object.keys(newCounts)) {
      counts[c] = (counts[c] ?? 0) + newCounts[c];
    }
    return counts;
  }, {});

// Solve a particular pair, simulating n generations, return the counts, memoize the result
const solvePair = _.memoize(
  (p, n) => {
    const toInsert = rules.find((r) => r[0] === p)[1];
    const newPairs = [p[0] + toInsert, toInsert + p[1]];
    if (n === 1) {
      const cts = getCounts(
        // remove the second letter of each pair to avoid overcounting
        newPairs.map((p) => p[0]).join("")
      );
      return cts;
    }
    return combineCounts(newPairs.map((p) => solvePair(p, n - 1)));
  },
  // Cache key needs to be based on both p and n, default is just p
  (p, n) => `${p},${n}`
);
// Convert ABCD into ["AB", "BC", "CD"]
const pairs = _.zip(
  template.slice(0, template.length - 1).split(""),
  template.slice(1).split("")
);
const simulate = (gens) => {
  // Simulate each individual pair, combine all the counts
  const totals = combineCounts(
    pairs
      .map((p) => solvePair(p.join(""), gens))
      // no pair counts the last letter, so add it separately
      .concat({ [template[template.length - 1]]: 1 })
  );
  return _.max(_.values(totals)) - _.min(_.values(totals));
};
console.log(simulate(10));
console.log(simulate(40));
