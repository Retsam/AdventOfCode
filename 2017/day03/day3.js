const GOAL = 347991;
const directionsGenerator = function*() {
    while(true) {
        yield [+1, 0];
        yield [0, +1];
        yield [-1, 0];
        yield [0, -1];
    }
}

const stepsGenerator = function*() {
    let x = 0;
    while(true) {
        x += 1;
        yield x;
        yield x;
    }
}

const aroundGenerator = function*(pos) {
    for(let x of [-1, 0, 1])
        for(let y of [-1, 0, 1])
            if(x !== 0 || y !== 0) yield move(pos, [x, y]);
}

function move(pos, dir) {
    return [pos[0] + dir[0], pos[1] + dir[1]];
}

function key(pos) {
    return `${pos[0]},${pos[1]}`;
}


function main() {
    let pos = [0, 0];
    let map = new Map();
    map.set(key(pos), 1);
    const stepsGen = stepsGenerator();
    const dirsGen = directionsGenerator();
    while(true) {
        const numSteps = stepsGen.next().value;
        const dir = dirsGen.next().value;
        for(let i = 0; i < numSteps; i++) {
            pos = move(pos, dir);
            console.log("Now at " + pos);
            let sum = 0;
            for(let neighbor of aroundGenerator(pos)) {
                if(map.has(key(neighbor))) {
                    sum += map.get(key(neighbor));
                }
            }
            console.log("Sum is " + sum);
            if(sum > GOAL) {
                return;
            }
            map.set(key(pos), sum);
        }
    }
}

