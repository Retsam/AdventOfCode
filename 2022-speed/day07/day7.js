const fs = require("fs");

//*
const path = "input.txt";
/*/
const path = "example.txt";
//*/

const input = fs.readFileSync(path).toString().trim().split("\n");

let dirs = [];
let dirTree = {};

while (input.length) {
  let command = input.shift();
  if (!command.startsWith("$")) throw new Error(command);
  command = command.slice("$ ".length);

  if (command.startsWith("cd")) {
    const dest = command.slice(3);
    if (dest === "/") dirs = [];
    else if (dest === "..") dirs = dirs.slice(0, dirs.length - 1);
    else dirs.push(dest);
  }
  if (command === "ls") {
    let currentPos = dirTree;
    for (const dir of dirs) {
      currentPos = currentPos[dir];
    }
    while (input[0] && !input[0].startsWith("$")) {
      const output = input.shift();
      if (output.startsWith("dir ")) {
        const folder = output.slice(4);
        currentPos[folder] = {};
      } else {
        const [size, file] = output.split(" ");
        currentPos[file] = parseInt(size);
      }
    }
  }
}

const sizesMap = {};
const registerSize = (dir, size) => {
  sizesMap[dir] = (sizesMap[dir] ?? 0) + size;
};
function calculateSizes(dir, breadcrumbs) {
  for (const [key, stuff] of Object.entries(dir)) {
    if (typeof stuff === "number") {
      for (const breadcrumb of breadcrumbs) {
        registerSize(breadcrumb, stuff);
      }
    } else {
      calculateSizes(
        stuff,
        breadcrumbs.concat(`${breadcrumbs[breadcrumbs.length - 1]}/${key}`)
      );
    }
  }
}

calculateSizes(dirTree, [""]);

const sizes = Object.values(sizesMap);

console.log(sizes.filter((x) => x <= 100000).reduce((a, b) => a + b));

// need 30M free out of 70M total, so can have 40M total
const needed = sizesMap[""] - 40000000;

console.log(sizes.filter((x) => x >= needed).sort((a, b) => b - a)[0]);
