use std::fs::File;
use std::io::prelude::*;

const INPUT_FILE: &'static str = "input.txt";

struct MarkerInfo {
    count: usize,
    times: usize
}

fn parse_marker(chars: &mut Iterator<Item=char>) -> MarkerInfo {
    let mut inside_parens = String::new();
    loop {
        let next_char = chars.next().unwrap();
        if next_char == ')' { break }
        inside_parens.push(next_char);
    }

    let mut dims = inside_parens.split("x");
    let count = dims.next().unwrap().parse::<usize>().unwrap();
    let times = dims.next().unwrap().parse::<usize>().unwrap();
    MarkerInfo {count:count, times:times}
}

fn decompress(s: &str) -> usize {
    let mut len = 0;
    let mut rest_of_string = s.to_string();
    while let Some(i) = rest_of_string.find('(') {
        len += i;
        rest_of_string = {
            let mut chars = rest_of_string.chars().skip(i+1); //+1 to skip the left paren
            let marker_info = parse_marker(&mut chars);

            let mut repeated_string = String::new();
            for _ in 0..marker_info.count {
                repeated_string.push(chars.next().unwrap());
            }
            let repeated_inner_len = decompress(repeated_string.as_str());
            len += marker_info.times * repeated_inner_len;

            chars.collect()
        }
    }
    len += rest_of_string.len();
    println!("{} decompressed to {}", s, len);
    len
}

fn main() {
    let mut f = File::open(INPUT_FILE).unwrap();
    let mut input = String::new();
    f.read_to_string(&mut input).unwrap();
    // let mut chars = input.trim().chars();

    // let mut decompressed_len = 0;
    //
    // let mut maybe_next_char = chars.next();
    // while let Some(c) = maybe_next_char {
    //     if c == '(' {
    //         let marker_info = parse_marker(&mut chars);
    //         decompressed_len += marker_info.count * marker_info.times;
    //         maybe_next_char = chars.nth(marker_info.count);
    //     } else {
    //         decompressed_len += 1;
    //         maybe_next_char = chars.next();
    //     }
    // }

    let decompressed_len = decompress(input.trim());
    // let decompressed_len = decompress("ABC");

    println!("Decompressed length is {}", decompressed_len);
}
