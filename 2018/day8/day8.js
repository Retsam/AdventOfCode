const _ = require("lodash");
const fs = require("fs");

const input = fs.readFileSync("input.txt").toString().trim();

const data = input.split(" ")
    .map(n => parseInt(n));

let cursor = 0;
const readValue = () => data[cursor++];

let metadataSum = 0;
const parseMetadata = () => {
    const value = readValue();
    metadataSum += value;
    return value;
}

const parseNode = () => {
    const nodeCount = readValue();
    const metadataCount = readValue();
    const nodes = _.times(nodeCount, parseNode)
    const metaData = _.times(metadataCount, parseMetadata);
    if(nodeCount === 0) {
        return _.sum(metaData);
    }
    return _.sum(metaData.map(m => nodes[m - 1]))
}

const rootValue = parseNode();
console.log(metadataSum); // Part one
console.log(rootValue); // Part two
