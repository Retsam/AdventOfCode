use std::io::{BufReader};
use std::fs::File;
use std::io::prelude::*;

/* next_key keypad looks like:
1 2 3
4 5 6
7 8 9
*/

// fn next_key(key: i32, dir: char) -> i32 {
//     match dir {
//         'U' => if key < 4      {key} else {key - 3},
//         'D' => if key > 6      {key} else {key + 3},
//         'L' => if key % 3 == 1 {key} else {key - 1},
//         'R' => if key % 3 == 0 {key} else {key + 1},
//         _ => panic!("Unexpected string! {}", dir)
//     }
// }

/* next_key2 Keypad looks like:
    1
  2 3 4
5 6 7 8 9
  A B C
    D
*/

const TOP_KEYS: [i32; 5] = [1, 2, 4, 5, 9];
const LFT_KEYS: [i32; 5] = [1, 2, 5, 10, 13];
const RGT_KEYS: [i32; 5] = [1, 4, 9, 12, 13];
const BTM_KEYS: [i32; 5] = [5, 9, 10, 12, 13];

fn next_key2(key: i32, dir: char) -> i32 {
    match dir {
        'U' => if TOP_KEYS.iter().any(|x| *x == key) {key} else {
            if key == 3 || key == 13 {key - 2} else {key - 4}
        },
        'D' => if BTM_KEYS.iter().any(|x| *x == key) {key} else {
            if key == 1 || key == 11 {key + 2} else {key + 4}
        },
        'L' => if LFT_KEYS.iter().any(|x| *x == key) {key} else {key - 1},
        'R' => if RGT_KEYS.iter().any(|x| *x == key) {key} else {key + 1},
        _ => panic!("Unexpected string! {}", dir)
    }
}

fn main() {
    let input_file = File::open("input.txt").unwrap();
    let input = BufReader::new(input_file);

    let mut key = 5;

    for _line in input.lines() {
        let line = _line.unwrap();
        for dir in line.chars() {
            // println!("{}", dir);
            key = next_key2(key, dir);
            // println!("{}", key);
        }
        // println!("DONE!");
        println!("{}", key);
    }
}
