use core::num::ParseIntError;
use core::str::FromStr;
use std::io::{self, Read};

const PART_1: bool = false;

fn gcd(x: i64, y: i64) -> i64 {
    let mut x = x;
    let mut y = y;
    while y != 0 {
        let t = y;
        y = x % y;
        x = t;
    }
    x
}
fn lcm(x: i64, y: i64) -> i64 {
    x * y / gcd(x, y)
}

#[derive(Debug, Clone)]
struct Point3 {
    x: i64,
    y: i64,
    z: i64,
}
impl FromStr for Point3 {
    type Err = ParseIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let coords = s.trim_matches(|p| p == '<' || p == '>')
            .split(", ")
            .map(|s| {&s[2..]})
            .collect::<Vec<_>>();
        let x = coords[0].parse::<i64>()?;
        let y = coords[1].parse::<i64>()?;
        let z = coords[2].parse::<i64>()?;

        Ok(Point3 { x, y, z })
    }
}

#[derive(Debug, Clone)]
struct MoonData {
    pos: Point3,
    vel: Point3,
}

fn apply_gravity(moons: &mut Vec<MoonData>) {
    let moons_copy = moons.clone();
    for moon in moons {
        for moon2 in moons_copy.iter() {
            moon.vel.x += (moon2.pos.x - moon.pos.x).signum();
            moon.vel.y += (moon2.pos.y - moon.pos.y).signum();
            moon.vel.z += (moon2.pos.z - moon.pos.z).signum();
        }
    }
}
fn apply_velocity(moons: &mut Vec<MoonData>) {
    for moon in moons {
        moon.pos.x += moon.vel.x;
        moon.pos.y += moon.vel.y;
        moon.pos.z += moon.vel.z;
    }
}

fn calc_energy(moon: &MoonData) -> i64 {
    let potential = moon.pos.x.abs() + moon.pos.y.abs() + moon.pos.z.abs();
    let kinetic = moon.vel.x.abs() + moon.vel.y.abs() + moon.vel.z.abs();
    potential * kinetic
}


fn find_cycle(positions: Vec<i64>) -> i64 {
    let mut i = 0;
    let mut state = positions.iter().cloned().map(|pos| (pos, 0)).collect::<Vec<_>>();
    let init_state = state.clone();
    loop {
        i += 1;
        let state2 = state.clone();
        for (pos, vel) in state.iter_mut() {
            for (pos2, _) in state2.iter() {
                *vel += (*pos2 - *pos).signum();
            }
        }
        for (pos, vel) in state.iter_mut() {
            *pos += *vel
        }
        if state == init_state {
            break i;
        }
    }
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let mut moons = buffer.lines()
        .map(|line| {
            MoonData {
                pos: line.parse::<Point3>().unwrap(),
                vel: Point3 { x: 0, y: 0, z: 0 },
            }
        })
        .collect::<Vec<_>>();
    if PART_1 {
        for _ in 0..1000 {
            apply_gravity(&mut moons);
            apply_velocity(&mut moons);
        }
        let energy: i64 = moons.iter().map(calc_energy).sum();
        println!("Total energy is {}", energy);

    } else {
        let x_cycle = find_cycle(moons.iter().map(|moon| moon.pos.x).collect());
        let y_cycle = find_cycle(moons.iter().map(|moon| moon.pos.y).collect());
        let z_cycle = find_cycle(moons.iter().map(|moon| moon.pos.z).collect());
        let cycle = lcm(lcm(x_cycle, y_cycle), z_cycle);
        println!("Cycles after {}", cycle);
    }
    Ok(())
}
