import fs from "fs";
//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/

const inputFile = fs.readFileSync(path).toString().trim();
const input = inputFile
  .split("\n\n")
  .map((x) => x.split("\n").map((x) => JSON.parse(x) as Num));

type Num = number | Num[];

function compare(l: Num, r: Num): number {
  if (typeof l !== typeof r) {
    return compare(
      typeof l === "number" ? [l] : l,
      typeof r === "number" ? [r] : r
    );
  }
  if (typeof l === "number") {
    if (r === l) return 0;
    return l - (r as number);
  }
  for (let i = 0; i < l.length; i++) {
    const rr = (r as Num[])[i];
    const ll = (l as Num[])[i];
    if (rr === undefined) return 1;
    const cmp = compare(ll, rr);
    if (cmp !== 0) return cmp;
  }
  return -1;
}

console.log(
  input.map(([x, y], i) => (compare(x, y) ? i + 1 : 0)).reduce((a, b) => a + b)
);

const allInput = inputFile
  .split("\n")
  .filter((x) => x.trim().length)
  .concat(["[[6]]", "[[2]]"])
  .map((x) => JSON.parse(x) as Num)
  .sort(compare)
  .map((x) => JSON.stringify(x));
console.log((allInput.indexOf("[[2]]") + 1) * (allInput.indexOf("[[6]]") + 1));
