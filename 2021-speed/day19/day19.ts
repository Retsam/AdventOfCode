import * as fs from "fs";
import * as nodePath from "path";
import * as _ from "lodash";
import { dir } from "console";

/*
const path = "input.txt";
/*/
const path = "example.txt";
//*/
let scanners = fs
  .readFileSync(nodePath.join(__dirname, path))
  .toString()
  .trim()
  .split("\n\n")
  .map((scan) => {
    return scan
      .split("\n")
      .slice(1)
      .map((x) => x.split(",").map((x) => +x));
  }) as Coord[][];

type Coord = [number, number, number];
const sq = (x) => Math.pow(x, 2);
const { abs } = Math;
const dists = ([c1, c2]: [Coord, Coord]) => [
  c1[0] - c2[0],
  c1[1] - c2[1],
  c1[2] - c2[2],
];

const dist2 = ([c1, c2]: [Coord, Coord]) => [
  sq(c1[0] - c2[0]) + sq(c1[1] - c2[1]) + sq(c1[2] - c2[2]),
];

const buildDists = (scan: Coord[]) => {
  const indexes = _.range(scan.length);
  const pairs = indexes
    .flatMap((l1, i, arr) => arr.map((l2) => [l1, l2]))
    .filter(([a, b]) => a !== b);

  const dists = pairs.map(([i1, i2]) => [
    [i1, i2],
    dist2([scan[i1], scan[i2]]),
  ]);
  return dists;
};

function relativePosition(i0, i1) {
  const s0Dists = buildDists(scanners[i0]);
  const s1Dists = buildDists(scanners[i1]);

  const eq = (a, b) => a.toString() === b.toString();

  const common0 = _.intersectionBy(s0Dists, s1Dists, ([_, v]) => v.toString());
  const common1 = _.intersectionBy(s1Dists, s0Dists, ([_, v]) => v.toString());
  const beacons1 = _.uniq(common1.flatMap(([x]) => x));
  const mappingToZero: Record<number, number[]> = {};
  for (const pair of common1) {
    const correspond = common0.filter(([_, v]) => eq(pair[1], v));
    if (correspond.length !== 1) throw new Error("uh oh");
    const [[b2]] = correspond;
    for (const b1 of pair[0]) {
      mappingToZero[b1] = mappingToZero[b1]
        ? _.intersection(mappingToZero[b1], b2)
        : b2;
    }
  }
  const finalMap = beacons1.map((b) => {
    if (!mappingToZero[b] || mappingToZero[b].length !== 1)
      throw new Error(`Failed to map ${b}`);
    return [b, mappingToZero[b][0]];
  });
  const mapping1 = finalMap[2];
  const mapping2 = finalMap[1];

  const directionMapping = {} as Record<
    0 | 1 | 2,
    readonly ["+" | "-", 0 | 1 | 2]
  >;

  const nodeAFor0 = scanners[i0][mapping1[1]];
  const nodeBFor0 = scanners[i0][mapping2[1]];
  const nodeAFor1 = scanners[i1][mapping1[0]];
  const nodeBFor1 = scanners[i1][mapping2[0]];

  const dists1 = dists([nodeAFor1, nodeBFor1]);
  const dists0 = dists([nodeAFor0, nodeBFor0]);

  function determineDir(d: 0 | 1 | 2) {
    directionMapping[d] = (
      {
        [dists0[0]]: ["+", 0],
        [-dists0[0]]: ["-", 0],
        [dists0[1]]: ["+", 1],
        [-dists0[1]]: ["-", 1],
        [dists0[2]]: ["+", 2],
        [-dists0[2]]: ["-", 2],
      } as const
    )[dists1[d]];
  }
  determineDir(0);
  determineDir(1);
  determineDir(2);
  console.log(directionMapping);

  const getSign = (d: 0 | 1 | 2) => (directionMapping[d][0] === "+" ? 1 : -1);
  const getDir = (n, d: 0 | 1 | 2) => {
    const [sign, idx] = directionMapping[d];
    return n[idx] * (sign === "+" ? 1 : -1);
  };

  function calDistance(n1, n0, d: 0 | 1 | 2) {
    const [sign, idx] = directionMapping[d];
    const nodeAX = n1[idx] * (sign === "+" ? 1 : -1);
    return nodeAX - n0[idx];
  }
  const deltas = [
    calDistance(nodeAFor1, nodeAFor0, 0),
    calDistance(nodeAFor1, nodeAFor0, 1),
    calDistance(nodeAFor1, nodeAFor0, 2),
  ];
  console.log(deltas);

  const newCoords = scanners[i1].map(([x, y, z]) => {
    const [dx, dy, dz] = [
      getDir(deltas, 0),
      getDir(deltas, 1),
      getDir(deltas, 2),
    ];
    const mapped = [
      (x - dx) * getSign(0),
      (y - dy) * getSign(1),
      (z - dz) * getSign(2),
    ] as Coord;
    return mapped;
  });
  scanners[i1] = _.uniq(scanners[i1].concat(newCoords));
}
const oneTo0 = relativePosition(0, 1);

const fourTo1 = relativePosition(0, 4);
// console.log(fourTo1);

// console.log(common0, common1, beacons1);

// console.log(
//   _.intersectionBy(s0Dists, s1Dists, ([_, v]) => v.toString()).flatMap(
//     ([x]) => x
//   )
// );
