const _ = require("lodash");

const playerCount = 10;
const lastMarble = 72164;

let currentMarble = 0;
let currentPlayer = -1;
let currentIndex = 0;
let circle = [];

const playerScores = {};
for(let p = 0; p<playerCount; p++) {
    playerScores[p] = 0;
}

const nextIndex = () => {
    currentIndex = (currentIndex + 2) % circle.length;
    currentIndex = currentIndex || circle.length
    return currentIndex;
}

const placeMarble = (num) => {
    // if(num % 10000 === 0) { console.log(num) }
    if(num === 0 || num % 23 !== 0) {
        const i = nextIndex();
        circle.splice(i, 0, currentMarble)
    } else {
        // playerScores[currentPlayer] += currentMarble;
        const iToRemove = (currentIndex - 7 + circle.length) % circle.length;
        currentIndex = iToRemove;
        const marble = circle.splice(iToRemove, 1);
        // playerScores[currentPlayer] += marble[0];
    }

    currentMarble++;
    // console.log(`${currentPlayer + 1} ${JSON.stringify(circle)}`)
    currentPlayer = (currentPlayer + 1 ) % playerCount
}

_.times(lastMarble + 1, placeMarble);
// console.log(playerScores)

// console.log(_.maxBy(Object.entries(playerScores), ([k, v]) => v));
