use std::io::{BufReader};
use std::fs::File;
use std::io::prelude::*;
use std::iter::FromIterator;


const FILE_NAME: &'static str = "input.txt";

fn is_valid(a: i32, b: i32, c: i32) -> bool {
    a + b > c &&
    a + c > b &&
    b + c > a
}

fn parse_line(line: &str) -> Vec<i32> {
    Vec::from_iter(line.split_whitespace().map(|x| x.parse::<i32>().unwrap()))
}

//Version 1: Rows
// fn main() {
//     let input_file = File::open(FILE_NAME).unwrap();
//     let input = BufReader::new(input_file);
//
//     let mut valid_count = 0;
//
//     for _line in input.lines() {
//         let line = _line.unwrap().trim().to_string();
//         let numbers = parse_line(line);
//         println!("{:?}", numbers);
//
//         if is_valid(numbers[0], numbers[1], numbers[2]) {
//             valid_count += 1
//         }
//     }
//     println!("{} are valid", valid_count);
// }

//Version 2: Columns
fn main() {
    let input_file = File::open(FILE_NAME).unwrap();
    let input = BufReader::new(input_file);

    let mut valid_count = 0;

    let mut line_iter = input.lines();

    loop {
        let l1;
        match line_iter.next() {
            None => break,
            Some(line) => {l1 = line.unwrap()}
        }
        let l2 = line_iter.next().unwrap().unwrap();
        let l3 = line_iter.next().unwrap().unwrap();

        let r1 = parse_line(&l1);
        let r2 = parse_line(&l2);
        let r3 = parse_line(&l3);

        if is_valid(r1[0], r2[0], r3[0]) { valid_count += 1 }
        if is_valid(r1[1], r2[1], r3[1]) { valid_count += 1 }
        if is_valid(r1[2], r2[2], r3[2]) { valid_count += 1 }

    }

    println!("{} are valid", valid_count);
}
