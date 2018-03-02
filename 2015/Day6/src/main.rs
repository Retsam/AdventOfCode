use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use std::cmp::max;


const INPUT_FILE: &'static str = "input.txt";

#[derive(Debug)]
enum Command {
    On,
    Off,
    Toggle
}

fn parse_line(line: &str) -> (Command, [usize; 2], [usize; 2]) {
    let words = line.split(" ").collect::<Vec<&str>>();
    let is_toggle = words[0] == "toggle";
    let command_str = if is_toggle { "toggle" } else { words[1] };
    let command = match command_str {
        "on" => Command::On,
        "off" => Command::Off,
        "toggle" => Command::Toggle,
        _ => panic!("Unexpected command_str {}", command_str)
    };
    let from = if is_toggle { parse_coords(words[1]) } else { parse_coords(words[2]) };
    let to = if is_toggle { parse_coords(words[3]) } else { parse_coords(words[4]) };
    (command, from, to)
}

fn parse_coords(coords_str: &str) -> [usize; 2] {
    let mut coords = coords_str.split(",").map(|s| s.parse::<usize>().unwrap());
    [coords.next().unwrap(), coords.next().unwrap()]
}

fn main() {
    let f = File::open(INPUT_FILE).unwrap();
    let input = BufReader::new(f);

    // let mut lights: Vec<bool> = vec![false; 1_000_000];
    let mut lights: Vec<i32> = vec![0; 1_000_000];

    for l in input.lines() {
        let (command, from, to) = parse_line(&l.unwrap());

        for x in from[0]..to[0]+1 {
            for y in from[1]..to[1]+1 {
                let flat = x * 1000 + y;
                match command {
                    // Command::On  => lights[flat] = true,
                    Command::On  => lights[flat] += 1,
                    // Command::Off => lights[flat] = false
                    Command::Off => lights[flat] = max(0, lights[flat] - 1),
                    // Command::Toggle => lights[flat] = !lights[flat]
                    Command::Toggle => lights[flat] += 2
                }
            }
        }

        assert!(from[0] <= to[0], "{} > {}", from[0], to[0]);
        assert!(from[1] <= to[1], "{} > {}", from[1], to[1]);
    }

    // let on_count = lights.into_iter().filter(|x| *x).count();
    // println!("{} lights are on", on_count);

    let brightness = lights.into_iter().fold(0, |a,b| a + b);
    println!("Total brightness is {}", brightness);

}
