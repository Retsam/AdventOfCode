use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::iter::FromIterator;

const FILE_NAME: &'static str = "input.txt";

#[derive(Debug)]
struct IPInfo<'a> {
    outer_segments: Vec<&'a str>,
    inner_segments: Vec<&'a str>
}

#[derive(PartialEq, Debug)]
struct ABA {
    a: char,
    b: char
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
    let abas = Vec::from_iter(ip_info.inner_segments.iter()
        .flat_map(|segment| get_abas(segment))
    );
    let babs = Vec::from_iter(ip_info.outer_segments.iter()
        .flat_map(|segment| get_abas(segment))
        .map(|aba| ABA {a: aba.b, b:aba.a})
    );
    for aba in abas {
        if babs.contains(&aba) {
            return true;
        }
    }
    false
}

fn get_abas(segment: &str) -> Vec<ABA> {
    let mut abas = vec![];
    for i in 0..segment.len()-2 {
        let mut chars = segment.chars().skip(i);
        let a1 = chars.next().unwrap();
        let b1 = chars.next().unwrap();
        let a2 = chars.next().unwrap();
        if a1 == a2 && a1 != b1 {
            abas.push(ABA {a: a1, b:b1})
        }
    }
    // println!("ABAS for {} are {:?}", segment, abas);
    abas
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
            println!("NO : {}", line)
        }
        // println!("{:?}", ip_info)
    }
    println!("{} addresses support TLS", supports_tls_count)
}
