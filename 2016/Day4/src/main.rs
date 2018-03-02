use std::io::{BufReader};
use std::fs::File;
use std::io::prelude::*;
use std::iter::FromIterator;
use std::collections::HashMap;

use std::cmp::Ordering;

struct RoomInfo {
    name: String,
    id: i32,
    checksum: String
}

fn parse_line(line: &str) -> RoomInfo {
    let checsum_rev : String = line.chars().rev().skip(1).take(5).collect();
    let checksum : String = checsum_rev.chars().rev().collect();
    let everything_else_rev: String = line.chars().rev().skip(7).collect();
    let everything_else: String = everything_else_rev.chars().rev().collect();

    let mut parts = Vec::from_iter(everything_else.split("-"));
    let id = parts.pop().unwrap().parse::<i32>().unwrap();
    let name : String = parts.into_iter().collect();

    RoomInfo {
        name: name,
        id: id,
        checksum: checksum
    }
}

#[derive(Debug)]
struct CharCount {
    character: char,
    count: i32
}

fn count_characters(text: &str) -> Vec<CharCount> {
    let mut char_counts = HashMap::new();
    for c in text.chars() {
        let counter = char_counts.entry(c).or_insert(0);
        *counter += 1;
    }
    Vec::from_iter(char_counts.into_iter().map(|(character, count)|
        CharCount{
            character:character,
            count: count
        })
    )
}

const FILE_NAME: &'static str = "input.txt";
fn main() {
    let input_file = File::open(FILE_NAME).unwrap();
    let input = BufReader::new(input_file);

    let mut sum = 0;

    for _line in input.lines() {
        let line = _line.unwrap().trim().to_string();

        let info = parse_line(&line);

        let mut char_counts = count_characters(&info.name);

        char_counts.sort_by(|a, b| {
            let order_by_count = b.count.cmp(&a.count);
            if let Ordering::Equal = order_by_count {
                a.character.cmp(&b.character)
            } else {
                order_by_count
            }
        });

        let correct_checksum: String = char_counts.iter().take(5).map(|char_count| char_count.character).collect();

        if correct_checksum != info.checksum {
            continue;
        }
    }
    println!("Sum is {}", sum)
}
