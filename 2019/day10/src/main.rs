use std::io::{self, Read};
use core::f32::consts::PI;
type Coord = (usize, usize);

fn calc_slope((x1, y1): &Coord, (x2, y2): &Coord) -> f32 {
    let dy = *y2 as f32 - *y1 as f32;
    let dx = *x2 as f32 - *x1 as f32;
    let slope = dy.atan2(dx);
    if slope < -PI/2. {
        slope + 2.*PI
    } else { slope }
}
fn dist2((x1, y1): &Coord, (x2, y2): &Coord) -> i32 {
    ((*x1 as i32) - (*x2 as i32)).abs() + ((*y1 as i32) - (*y2 as i32)).abs()
}

fn score_location(this: &Coord, asteroids: &[Coord]) -> usize {
    let mut slopes = Vec::new();
    for other in asteroids {
        if this == other {
            continue;
        }
        let slope = calc_slope(this, other);
        if !slopes.contains(&slope) {
            slopes.push(slope);
        }
    }
    slopes.len()
}

struct AsteroidVaporizer {
    base: Coord,
    current_pass: Vec<Coord>,
    remaining: Vec<Coord>,
}

impl AsteroidVaporizer {
    fn new(base: Coord, mut asteroids: Vec<Coord>) -> AsteroidVaporizer {
        asteroids.retain(|o| *o != base);
        asteroids.sort_by_cached_key(|o| dist2(&o, &base));
        AsteroidVaporizer {
            base,
            current_pass: vec!(),
            remaining: asteroids
        }
    }
    fn ready_for_next_pass(&mut self) {
        if self.current_pass.len() > 0 {
            return;
        }
        let mut slopes = Vec::new();
        let mut next_pass_slopes = Vec::new();

        for other in self.remaining.iter() {
            let slope = calc_slope(&self.base, &other);
            if !slopes.contains(&slope) {
                slopes.push(slope);
                next_pass_slopes.push((*other, slope));
            }
        }

        next_pass_slopes.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        let next_pass = next_pass_slopes.into_iter().map(|x| {
            x.0
        }).collect::<Vec<_>>();
        self.remaining.retain(|c| { !next_pass.contains(&c) });
        self.current_pass = next_pass;
    }
}
impl Iterator for AsteroidVaporizer {
    type Item = Coord;
    fn next(&mut self) -> Option<Coord> {
        self.ready_for_next_pass();
        self.current_pass.pop()
    }
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let mut coords = Vec::new();
    for (y, line) in buffer.lines().enumerate() {
        for (x, chr) in line.chars().enumerate() {
            if chr == '#' {
                coords.push((x, y));
            }
        }
    }
    let best_location = coords
        .iter()
        .max_by_key(|coord| score_location(coord, coords.as_slice()))
        .unwrap();

    println!("Best location is {:?} with {}", best_location, score_location(best_location, coords.as_slice()));

    let mut vaporizer = AsteroidVaporizer::new(*best_location, coords);
    let two_hundredth = vaporizer.nth(199).expect("Expected a 200th asteroid");
    println!("Two hundredth astroid destroyed is {:?}", two_hundredth);

    Ok(())
}
