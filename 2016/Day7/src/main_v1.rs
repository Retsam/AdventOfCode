use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

const FILE_NAME: &'static str = "input.txt";

#[derive(Debug)]
struct IPInfo<'a> {
    outer_segments: Vec<&'a str>,
    inner_segments: Vec<&'a str>
}

fn parse_line(line: &str) -> IPInfo {
    let mut inner_segments: Vec<&str> = vec![];
    let mut outer_segments: Vec<&str> = vec![];
    let chars = line.chars();

    let mut current_slice_start = 0;
    for (next_char, i) in chars.zip(0..) {
        match next_char {
            '[' => {
                let slice = &line[current_slice_start..i];
                if slice.len()>0 {outer_segments.push(slice)}
                current_slice_start = i+1;
            },
            ']' => {
                let slice = &line[current_slice_start..i];
                if slice.len()>0 {inner_segments.push(slice)}
                current_slice_start = i+1;
            },
            _ => {}
        }
    }
    let slice = &line[current_slice_start..line.len()];
    if slice.len()>0 {outer_segments.push(slice)}

    IPInfo {inner_segments: inner_segments, outer_segments: outer_segments}
}

fn supports_tls(ip_info: &IPInfo) -> bool {
    for segment in &ip_info.inner_segments {
        if has_abba(segment) {
            return false
        }
    }
    for segment in &ip_info.outer_segments {
        if has_abba(segment) {
            return true
        }
    }
    false
}

fn has_abba(segment: &str) -> bool {
    for i in 0..segment.len()-3 {
        let a1 = &segment[i..i+1];
        let b1 = &segment[i+1..i+2];
        let b2 = &segment[i+2..i+3];
        let a2 = &segment[i+3..i+4];
        if a1 == a2 && b1 == b2 && a1 != b1{
            return true;
        }
    }
    false
}

fn main() {
    let f = File::open(FILE_NAME).unwrap();
    let i = BufReader::new(f);

    let lines = i.lines().map(|l| l.unwrap());

    let mut supports_tls_count = 0;

    for line in lines {
        let ip_info = parse_line(&line);
        if supports_tls(&ip_info) {
            println!("YES: {}", line);
            supports_tls_count += 1;
        } else {
            println!("NO: {}", line)
        }
        // println!("{:?}", ip_info)
    }
    println!("{} addresses support TLS", supports_tls_count)
}
