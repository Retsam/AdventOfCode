import fs from "fs";
import _ from "lodash";

type Point = [number, number];

const points = fs.readFileSync("input.txt").toString()
    .split("\n").map(x => x.split(", ").map(x => parseInt(x))) as Point[];

const THRESHOLD = 10000;

const minX = _.minBy(points, ([x,_]) => x)![0]
const minY = _.minBy(points, ([_,y]) => y)![1]
const maxX = _.maxBy(points, ([x,_]) => x)![0]
const maxY = _.maxBy(points, ([_,y]) => y)![1]

const distance = (p1: Point, p2: Point) => {
    return Math.abs(p1[0] - p2[0]) + Math.abs(p1[1] - p2[1]);
}

function main() {
    console.log("Part 1", part1());
    console.log("Part 2", part2());
}

// ========
// Part Two
// ========
function part2() {
    let count = 0;;

    const FUZZ = 10
    for(let x=minX-FUZZ; x<maxX+FUZZ; x++) {
        for(let y=minY-FUZZ; y<maxY+FUZZ; y++) {
            if(totalDist([x, y]) < THRESHOLD) {
                count++
            }
        }
    }
    return count;
}


const totalDist = (p: Point) => (
    _.sum(points.map(p2 => distance(p, p2)))
)


// ========
// Part One
// ========
function part1() {
    const infinitePoints = findInfinitePoints();
    const nonInfinitePoints = points.filter(point => !infinitePoints.has(point))
    const score = new Map();
    nonInfinitePoints.forEach(p => score.set(p, 0));

    for(let x=minX; x<maxX; x++) {
        for(let y=minY; y<maxY; y++) {
            const c = closest([x, y]);
            if(score.has(c)) {
                score.set(c, score.get(c) + 1)
            }
        }
    }

    const winner = _.maxBy(Array.from(score.entries()), ([a, b]) => b)!;
    return winner[1];
}

const closest = (p: Point) => {
    const close = _.minBy(points, p2 => distance(p, p2))!;
    const secondBest = _.minBy(_.without(points, close), p2 => distance(p, p2))!;
    if(distance(p, close) === distance(p, secondBest)) {
        return null;
    }

    return close;
}

const findInfinitePoints = () => {
    const infinitePoints = new Set();
    for(let x=minX; x<=maxX; x++) {
        infinitePoints.add(closest([x, minY]))
        infinitePoints.add(closest([x, maxY]))
    }
    for(let y=minY; y<=maxY; y++) {
        infinitePoints.add(closest([minX, y]))
        infinitePoints.add(closest([maxX, y]))
    }
    return infinitePoints;
}

main();
