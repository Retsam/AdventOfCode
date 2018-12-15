const _ = require("lodash");
const fs = require("fs");

const input = fs.readFileSync("input.txt").toString().trim();

const data = input.split("\n");

const initialState = data[0].slice("initial state: ".length).split("");
initialState.min = 0;

const instructions = _.fromPairs(data.slice(2).map(line => line.split(" => ")))

function simulate(state) {
    const get = (i) => state[i] || '.';
    let newState = [];
    newState.min = state.min-2;
    for(let i=newState.min; i<state.length+2; i++) {
        let slice = get(i-2) + get(i-1) + get(i) + get(i+1) + get(i+2)
        newState[i] = instructions[slice] || '.';
    }
    while(newState.min < 0 && newState[newState.min] == ".") newState.min++;
    while(newState.length > 0 && newState[newState.length-1] == ".") newState.length--;
    return newState;
}

let state = initialState;
let prevScore = calcScore(initialState);
console.log(prevScore - 20 * 15);
const sample = 200;
for(let i = 0; i < sample; i++) {
    if(i % 100000000 === 0) console.log(i);
    state = simulate(state);
    const score = calcScore(state)
    console.log(score - prevScore);
    prevScore = score
}

console.log("Score after sample is " + prevScore);
console.log("Expected prevScore = ", prevScore + 15 * (50000000000 - sample))


function calcScore(state) {
    let score = 0;
    for(let i=state.min; i<state.length+2; i++) {
        if(state[i] === "#") score+=i
    }
    return score;
}
console.log(calcScore(state));
