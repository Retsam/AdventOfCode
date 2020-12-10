const _ = require("lodash");
const fs = require("fs");

/*
const file = "example.txt";
/*/
const file = "input.txt";
// */

const text = fs.readFileSync(file).toString().trim();

const lines = text.split("\n");
let validCount = 0;
let passport = {};
let line = lines.shift();

function validHeight(hgt) {
    const match = (hgt || "").match(/^(\d+)(in|cm)$/);
    if(!match) return false;
    return match[2] === "cm" ? match[1] >= 150 && match[1] <= 193 :
        match[1] >= 59 && match[1] <=76;

}

const eyec = "amb blu brn gry grn hzl oth".split(" ");

while(line !== undefined) {
    let fields = line.split(" ");
    fields.forEach(field => {
        const [key, value] = field.split(":");
        passport[key] = value;
    })
    if(line === "") {
        if(
            passport.byr >= 1920 && passport.byr <= 2002 &&
            passport.iyr >= 2010 && passport.iyr <= 2020 &&
            passport.eyr >= 2020 && passport.eyr <= 2030 &&
            validHeight(passport.hgt) &&
            /^#[0-9a-f]{6}$/.test(passport.hcl) &&
            /^[0-9]{9}$/.test(passport.pid) &&
            eyec.includes(passport.ecl)
        ) {
            validCount++;
        }
        passport = {};
    }
    line = lines.shift();
}

console.log(validCount);
