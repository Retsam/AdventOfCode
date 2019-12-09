use std::io::{self, Read};
use std::collections::HashMap;
use std::convert::TryInto;

type Dist = u32;
type Pos = i32;
type Count = u32;
type Step = (char, Dist);

const PART_2: bool = true;

fn position_map(steps: Vec<Step>) -> HashMap<(Pos, Pos), Count> {
    let mut pos_map = HashMap::new();
    let mut x = 0;
    let mut y = 0;
    let mut d = 1;
    for (dir, dist) in steps {
        for _ in 0..dist {
            x = x + if dir == 'R' { 1 } else if dir == 'L' { -1 } else { 0 };
            y = y + if dir == 'U' { 1 } else if dir == 'D' { -1 } else { 0 };
            pos_map.insert((x, y), d);
            d += 1;
        }
    }
    pos_map
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let mut lines = buffer.lines()
        .map(|s| s.split(",")
            .map(|s| {
                let mut chars = s.chars();
                let dir = chars.next().expect("Expected a dir");
                let dist = chars.collect::<String>().parse::<Dist>().expect("Not a number");
                (dir, dist)
            }).collect::<Vec<_>>()
        );

    let (p1, p2) = (lines.next().expect("Expected one"), lines.next().expect("Expected two"));
    let m1 = position_map(p1);
    let m2 = position_map(p2);
    let mut intersections: Vec<u32> = Vec::new();
    for (pos, t1) in &m1 {
        match m2.get(pos) {
            Some(t2) => {
                if PART_2 {
                    intersections.push(t1 + t2)
                } else {
                    intersections.push((pos.0.abs() + pos.1.abs()).try_into().unwrap());
                }
            }
            None => ()
        }
    }
    println!("Distance is {}", intersections.iter().cloned().fold(1000000, |x, y| x.min(y)));

    Ok(())
}
