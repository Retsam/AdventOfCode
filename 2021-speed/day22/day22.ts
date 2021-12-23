import * as fs from "fs";
import * as nodePath from "path";
import * as _ from "lodash";
import { initial, map } from "lodash";

/*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
let data = fs
  .readFileSync(nodePath.join(__dirname, path))
  .toString()
  .trim()
  .split("\n")
  .map(
    (l) =>
      [
        l[1] === "n",
        ...l.split(",").map((d) =>
          d
            .match(/(-?\d+)\.\.(-?\d+)/)
            .slice(1)
            .map((x) => +x)
        ),
      ] as [boolean, ...Bounds3]
  );

type Bound = [min: number, max: number];
type Bounds3 = [Bound, Bound, Bound];
type Coord = [number, number, number];
const toStr = (x: Coord) => x.join(",");
const onSet = new Set<string>();

// let i = 0;
// for (const line of data) {
//   const [on, xx, yy, zz] = line as ;
//   for (let x = Math.max(-50, xx[0]); x <= Math.min(50, xx[1]); x++) {
//     for (let y = Math.max(-50, yy[0]); y <= Math.min(50, yy[1]); y++) {
//       for (let z = Math.max(-50, zz[0]); z <= Math.min(50, zz[1]); z++) {
//         // console.log([x, y, z]);
//         if (on) {
//           onSet.add(toStr([x, y, z]));
//         } else {
//           onSet.delete(toStr([x, y, z]));
//         }
//       }
//     }
//   }
// }
// i;
// console.log(onSet.size);

// type Space = { bounds: Bounds3; subSpace: Space[] };

const containsDim = (a: Bound, one: Bound) => one[0] >= a[0] && one[1] <= a[1];

const contains = (a: Bounds3, one: Bounds3) =>
  [0, 1, 2].every((i) => containsDim(a[i], one[i]));

type Tree = {
  state: "on" | "off";
  radius: number;
  center: Coord;
  subTrees?: Partial<Record<0 | 1 | 2 | 3 | 4 | 5 | 6 | 7, Tree>>;
};

const noOverlap = (a: Bound, one: Bound) => one[0] >= a[1] || one[1] < a[0];
const getOverlap = (a: Bound, one: Bound): Bound => [
  Math.max(one[0], a[0]),
  Math.min(one[1], a[1]),
];
const intersect = ([ax, ay, az]: Bounds3, [x1, y1, z1]: Bounds3): Bounds3 => {
  console.log(`Checking intersection of ${[ax, ay, az]}, ${[x1, y1, z1]}`);
  if (noOverlap(ax, x1) || noOverlap(ay, y1) || noOverlap(az, z1))
    return undefined;
  console.log("overlaps");
  return [getOverlap(ax, x1), getOverlap(ay, y1), getOverlap(az, z1)];
};

const getRegions = ({ center: [cx, cy, cz], radius }: Tree): Bounds3[] => {
  let regions: Bounds3[] = [];
  // prettier-ignore
  for(const x of [[cx - radius, cx], [cx, cx + radius]] as Bound[]) {
        for(const y of [[cy - radius, cy], [cy, cy + radius]] as Bound[]) {
            for(const z of [[cz - radius, cz], [cz, cz + radius]] as Bound[]) {
                regions.push([x, y, z])
            }
        }
    }
  return regions;
};

const getBounds = ({ center: [cx, cy, cz], radius }: Tree): Bounds3 => [
  [cx - radius, cx + radius],
  [cy - radius, cy + radius],
  [cz - radius, cz + radius],
];
const addSection = (tree: Tree, toAdd: Bounds3, state: "on" | "off") => {
  //   console.log(
  //     `Adding section to tree (${tree.radius}, ${tree.center.join(",")})`
  //   );
  if (JSON.stringify(getBounds(tree)) === JSON.stringify(toAdd)) {
    tree.state = state;
    tree.subTrees = undefined;
    console.log("Replacing whole subtree");
    return;
  }
  getRegions(tree).forEach((region, i) => {
    const intersection = intersect(region, toAdd);
    if (intersection) {
      if (tree.subTrees === undefined) tree.subTrees = {};
      let subTree: Tree | undefined = tree.subTrees[i];

      if (!subTree) {
        const radius = Math.floor(tree.radius / 2);
        // prettier-ignore
        const center: Coord = [region[0][0] + radius, region[1][0] + radius, region[2][0] + radius];
        tree.subTrees[i] = subTree = {
          state: tree.state,
          radius,
          center,
        };
      }
      if (subTree.radius === 0) {
        subTree.state = state;
      } else {
        addSection(subTree, intersection, state);
      }
    }
  });
};

const calcSize = (
  { state, radius, subTrees = {} }: Tree,
  lookingFor: "on" | "off"
): number => {
  const subTreeSize = (state: "on" | "off") =>
    Object.values(subTrees)
      .map((subTree) => calcSize(subTree, state))
      .reduce((a, b) => a + b, 0);

  if (state === lookingFor) {
    return (
      radius * radius * 8 - subTreeSize(lookingFor === "on" ? "off" : "on")
    );
  }
  return subTreeSize(lookingFor);
};

const tree: Tree = {
  state: "off",
  radius: 64,
  center: [0, 0, 0],
  subTrees: {},
};
// for (const [isOn, ...bounds] of data) {
//   addSection(tree, bounds, isOn ? "on" : "off");
// }
// console.log(JSON.stringify(tree, null, 2));
// console.log(calcSize(tree, "on"));

const testTree: Tree = { state: "off", radius: 4, center: [0, 0, 0] };
addSection(
  testTree,
  [
    [0, 2],
    [0, 2],
    [0, 2],
  ],
  "on"
);
console.log(calcSize(testTree, "on"));
console.log(JSON.stringify(testTree, null, 2));
