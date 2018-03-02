use std::io::{BufReader};
use std::fs::File;
use std::io::prelude::*;
use std::collections::HashMap;
use std::iter::FromIterator;

const FILE_NAME: &'static str = "input.txt";
const MSG_LEN: usize = 8;

fn main() {
    let f = File::open(FILE_NAME).unwrap();
    let input = BufReader::new(f);

    let mut freq_maps: Vec<HashMap<char, i32>> = Vec::from_iter((0..MSG_LEN).map(|_| HashMap::new()));

    for l in input.lines() {
        let line = l.unwrap();
        for i in 0..MSG_LEN {
            let c = line.chars().nth(i).unwrap();
            let ref mut map = freq_maps[i];
            let counter = map.entry(c).or_insert(0);
            *counter += 1;
        }
    }

    let mut res = "".to_string();
    for map in freq_maps {
        // let (most_common, _) = map.iter().max_by_key(|&(_, count)| count).unwrap();
        // res.push(*most_common);
        let (least_common, _) = map.iter().min_by_key(|&(_, count)| count).unwrap();
        res.push(*least_common);
    }

    println!("Result is {}", res);
}
