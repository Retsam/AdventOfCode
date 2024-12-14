use std::{error, fs};

use colored::Colorize;
use itertools::Itertools;
use utils::bounds::Bounds;
use utils::coord::Coord;

const INPUT: (i64, i64, &str) = (101, 103, "input.txt");
// const INPUT: (i64, i64, &str) = (7, 11, "example.txt");

const WIDTH: i64 = INPUT.0;
const HEIGHT: i64 = INPUT.1;
const FILE: &str = INPUT.2;

#[derive(Debug, Clone)]
struct Robot {
    p: Coord,
    v: Coord,
}
fn parse_input(input: &str) -> Vec<Robot> {
    input
        .lines()
        .map(|line| {
            let (p, v) = line
                .splitn(2, " ")
                .map(|coord_str| {
                    let (x, y) = coord_str[2..]
                        .splitn(2, ",")
                        .map(|x| x.parse::<i64>().unwrap())
                        .collect_tuple()
                        .unwrap();
                    Coord { x, y }
                })
                .collect_tuple()
                .unwrap();
            Robot { p, v }
        })
        .collect()
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut p1 = 0;
    let buf = fs::read_to_string(FILE).map_err(|_| "Failed to read input")?;
    let input = parse_input(buf.trim());

    let mut robots = input.clone();

    let bounds = Bounds::new(WIDTH, HEIGHT);

    let debug = |robots: &Vec<Robot>| {
        bounds.debug(|c| {
            let n = robots.iter().filter(|r| r.p == c).count();
            if n == 0 {
                " ".to_string()
            } else {
                n.to_string().bright_green().to_string()
            }
        })
    };
    let mut i = 0;
    let p2 = loop {
        i += 1;
        update(&mut robots);
        if i == 100 {
            p1 = grid_score(&mut robots);
        }
        if robots.iter().unique_by(|r| r.p).count() == robots.len() {
            debug(&robots);
            break i;
        }
    };

    println!("Part 1: {p1}\nPart 2: {p2}");
    Ok(())
}

fn update(robot_map: &mut Vec<Robot>) {
    for robot in robot_map {
        let p = &mut robot.p;

        *p = p.mv(robot.v);
        p.x = p.x.rem_euclid(WIDTH);
        p.y = p.y.rem_euclid(HEIGHT);
    }
}

fn grid_score(robots: &mut Vec<Robot>) -> i64 {
    let mid_x = (WIDTH - 1) / 2;
    let mid_y = (HEIGHT - 1) / 2;

    let (mut a, mut b, mut c, mut d) = (0, 0, 0, 0);

    for Robot { p, .. } in robots {
        if p.x == mid_x || p.y == mid_y {
            continue;
        }
        let in_top = p.y < mid_y;
        let in_left = p.x < mid_x;
        match (in_left, in_top) {
            (true, true) => a += 1,
            (true, false) => b += 1,
            (false, true) => c += 1,
            (false, false) => d += 1,
        }
    }
    a * b * c * d
}
