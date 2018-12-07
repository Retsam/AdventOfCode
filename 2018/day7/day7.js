const lines = `Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.`.split("\n");

const lines2 = `Step B must be finished before step X can begin.
Step H must be finished before step P can begin.
Step Y must be finished before step J can begin.
Step Z must be finished before step I can begin.
Step T must be finished before step U can begin.
Step R must be finished before step C can begin.
Step S must be finished before step J can begin.
Step W must be finished before step J can begin.
Step C must be finished before step L can begin.
Step L must be finished before step F can begin.
Step E must be finished before step G can begin.
Step A must be finished before step G can begin.
Step V must be finished before step X can begin.
Step U must be finished before step O can begin.
Step P must be finished before step F can begin.
Step O must be finished before step I can begin.
Step I must be finished before step F can begin.
Step K must be finished before step F can begin.
Step J must be finished before step F can begin.
Step G must be finished before step X can begin.
Step M must be finished before step X can begin.
Step F must be finished before step Q can begin.
Step Q must be finished before step N can begin.
Step D must be finished before step N can begin.
Step X must be finished before step N can begin.
Step I must be finished before step Q can begin.
Step U must be finished before step I can begin.
Step D must be finished before step X can begin.
Step B must be finished before step W can begin.
Step L must be finished before step N can begin.
Step U must be finished before step X can begin.
Step U must be finished before step J can begin.
Step C must be finished before step V can begin.
Step G must be finished before step N can begin.
Step S must be finished before step K can begin.
Step Q must be finished before step D can begin.
Step J must be finished before step X can begin.
Step V must be finished before step K can begin.
Step Z must be finished before step A can begin.
Step L must be finished before step M can begin.
Step H must be finished before step D can begin.
Step V must be finished before step Q can begin.
Step L must be finished before step V can begin.
Step S must be finished before step D can begin.
Step C must be finished before step Q can begin.
Step S must be finished before step L can begin.
Step E must be finished before step V can begin.
Step E must be finished before step P can begin.
Step C must be finished before step I can begin.
Step O must be finished before step K can begin.
Step H must be finished before step V can begin.
Step M must be finished before step F can begin.
Step K must be finished before step N can begin.
Step C must be finished before step X can begin.
Step G must be finished before step D can begin.
Step E must be finished before step U can begin.
Step R must be finished before step L can begin.
Step K must be finished before step G can begin.
Step W must be finished before step C can begin.
Step B must be finished before step L can begin.
Step L must be finished before step J can begin.
Step U must be finished before step D can begin.
Step I must be finished before step G can begin.
Step Q must be finished before step X can begin.
Step B must be finished before step M can begin.
Step T must be finished before step P can begin.
Step G must be finished before step Q can begin.
Step Y must be finished before step U can begin.
Step M must be finished before step D can begin.
Step P must be finished before step I can begin.
Step I must be finished before step K can begin.
Step O must be finished before step M can begin.
Step H must be finished before step Z can begin.
Step V must be finished before step M can begin.
Step P must be finished before step J can begin.
Step B must be finished before step U can begin.
Step E must be finished before step X can begin.
Step M must be finished before step Q can begin.
Step W must be finished before step L can begin.
Step O must be finished before step J can begin.
Step I must be finished before step X can begin.
Step J must be finished before step N can begin.
Step Y must be finished before step S can begin.
Step E must be finished before step D can begin.
Step M must be finished before step N can begin.
Step E must be finished before step O can begin.
Step I must be finished before step D can begin.
Step V must be finished before step N can begin.
Step R must be finished before step X can begin.
Step Z must be finished before step O can begin.
Step O must be finished before step X can begin.
Step I must be finished before step J can begin.
Step S must be finished before step E can begin.
Step E must be finished before step Q can begin.
Step J must be finished before step Q can begin.
Step H must be finished before step Y can begin.
Step T must be finished before step G can begin.
Step S must be finished before step A can begin.
Step P must be finished before step K can begin.
Step A must be finished before step D can begin.
Step B must be finished before step P can begin.`.split('\n');

const input = lines.map(line => {
    const words = line.split(" ");
    return [words[1], words[7]];
})

const allSteps = _.uniq([].concat(...input)).sort();

const reqs = new Map();
for([s1, s2] of input) {
    const cReqs = reqs.get(s2) || [];
    reqs.set(s2, cReqs.concat(s1))
}

let order = "";
const finishedSteps = [];
while(finishedSteps.length < allSteps.length) {
    for(const step of _.without(allSteps, ...finishedSteps)) {
        const r = _.without(reqs.get(step), ...finishedSteps);
        if(!r.length) {
            finishedSteps.push(step);
            order += step;
        }
    }
}
console.log(order);



