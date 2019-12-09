const min = 138241;
const max = 674034;

const meetsCriteria = (x) => {
    const digits = x.toString().split("");
    const counts = Object.values(digits.reduce((counts, digit) => {
        counts[digit] = (counts[digit] || 0) + 1
        return counts;
    }, {}))
    return counts.find(x => x === 2) && digits.sort().join("") === x.toString();
}

let c = 0;
for(let x = min; x <= max; x++) {
    if(meetsCriteria(x)) { c++ }
}
console.log(meetsCriteria(123333));
console.log(c);
