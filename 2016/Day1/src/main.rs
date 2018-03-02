use std::fs::File;
use std::io::prelude::*;

#[derive(Debug, Copy, Clone)]
struct Pos {
    x: i32,
    y: i32
}

#[derive(Copy, Clone)]
enum Turn {
    Right,
    Left
}
impl Turn {
    fn fromString(s: &str) -> Turn {
        match s {
            "R" => Right,
            "L" => Left,
            _ => panic!("Unexpected turn direction {}", s)
        }
    }
}
#[derive(Copy, Clone, Debug)]
enum Dir {
    North,
    South,
    East,
    West
}

use Turn::*;
use Dir::*;

fn do_turn(dir: Dir, turn: Turn) -> Dir {
    match dir {
        North => match turn {
            Right => East,
            Left => West
        },
        East => match turn {
            Right => South,
            Left => North
        },
        South => match turn {
            Right => West,
            Left => East
        },
        West => match turn {
            Right => North,
            Left => South
        }
    }
}

fn go(pos: Pos, dir: Dir, steps: i32) -> Pos {
    // println!("Going {:?} {}", dir, steps);
    let new_pos = match dir {
        North => Pos {x: pos.x,         y: pos.y + steps},
        East  => Pos {x: pos.x + steps, y: pos.y},
        South => Pos {x: pos.x,         y: pos.y - steps},
        West  => Pos {x: pos.x - steps, y: pos.y}
    };
    // println!("Now at {:?}", new_pos);
    new_pos
}

fn main() {
    let mut input_file = File::open("input.txt").unwrap();

    let mut input_str = String::new();
    input_file.read_to_string(&mut input_str).unwrap();

    let mut pos = Pos {x: 0, y: 0};
    let mut dir = North;

    let mut pos_vec = Vec::new();
    pos_vec.push(pos);

    for i in input_str.trim().split(", ") {
        println!("{}", i);
        let turn = Turn::fromString(&i[0..1]);
        let steps = *&i[1..].parse::<i32>().unwrap();

        dir = do_turn(dir, turn);
        for _ in 0..steps {
            pos = go(pos, dir, 1);
            println!("New pos is {:?}", pos);

            match pos_vec.iter().find(|pos2| {
                pos2.x == pos.x && pos2.y == pos.y
            }) {
                Some(pos) => {
                    println!("MATCH!, {:?}", pos);
                    return
                }
                None => ()
            }
            pos_vec.push(pos)
        }
    }

    println!("No match found...");

    println!("{:?}", pos);
    println!("{}", pos.x + pos.y);
}
