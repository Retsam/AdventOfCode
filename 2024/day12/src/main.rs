use std::collections::HashSet;
use std::error;
use std::io::{self, Read};

use itertools::Itertools;
use utils::bounds::Bounds;
use utils::coord::Coord;
use utils::dir::{Dir, Neighbors};

struct Grid<T> {
    grid: Vec<Vec<T>>,
}
impl<T: Clone> Grid<T> {
    fn get(&self, c: Coord) -> Option<T> {
        self.grid
            .get(c.y as usize)
            .and_then(|row| row.get(c.x as usize).cloned())
    }
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let grid = buf
        .lines()
        .map(|line| line.chars().collect_vec())
        .collect_vec();

    let mut visited_set = HashSet::<Coord>::new();

    let bounds = Bounds::from_vec(&grid);
    let grid = Grid { grid };
    let (p1, p2) = bounds
        .iter()
        .map(|coord| {
            if visited_set.contains(&coord) {
                return (0, 0);
            }
            visited_set.insert(coord);

            let mut region_set = HashSet::new();
            let sym = grid.get(coord).unwrap();
            let mut peri = 0;
            flood(sym, coord, &mut region_set, &grid, &mut peri);

            let area = region_set.len() as u64;
            let sides = count_sides(&region_set);

            for i in region_set.into_iter() {
                visited_set.insert(i);
            }
            (area * peri, area * sides)
        })
        .fold((0, 0), |(p1, p2), (a, b)| (p1 + a, p2 + b));

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}

fn flood(
    sym: char,
    coord: Coord,
    region_set: &mut HashSet<Coord>,
    grid: &Grid<char>,
    peri: &mut u64,
) {
    if grid.get(coord) != Some(sym) {
        *peri += 1;
        return;
    }
    region_set.insert(coord);
    for n in coord.neighbors().iter() {
        if region_set.contains(n) {
            continue;
        }
        flood(sym, *n, region_set, grid, peri);
    }
}

fn count_sides(region: &HashSet<Coord>) -> u64 {
    let (x_min, x_max) = region.iter().map(|c| c.x).minmax().into_option().unwrap();
    let (y_min, y_max) = region.iter().map(|c| c.y).minmax().into_option().unwrap();

    [
        (
            (x_min..=x_max).map(|x| Coord::new(x, y_min)).collect_vec(),
            Dir::D,
        ),
        (
            (x_min..=x_max).map(|x| Coord::new(x, y_max)).collect_vec(),
            Dir::U,
        ),
        (
            (y_min..=y_max).map(|y| Coord::new(x_min, y)).collect_vec(),
            Dir::R,
        ),
        (
            (y_min..=y_max).map(|y| Coord::new(x_max, y)).collect_vec(),
            Dir::L,
        ),
    ]
    .into_iter()
    .map(|(coords, dir)| scan(region, coords, dir))
    .sum()
}

fn scan(region: &HashSet<Coord>, coords: Vec<Coord>, dir: Dir) -> u64 {
    let mut side_count = 0;
    let mut state = coords.into_iter().map(|c| (c, false)).collect_vec();

    loop {
        let new_sides = state
            .iter_mut()
            .map(|(c, inside)| {
                let prev_inside = *inside;
                *inside = region.contains(c);
                *c = c.mv(dir);
                // Did we just enter a region?
                *inside && !prev_inside
            })
            // Count the 'chunks' of true values as a single side
            .dedup()
            .filter(|x| *x)
            .count();

        side_count += new_sides as u64;

        // Once the entire line is outside we're done
        if state.iter().all(|(_, inside)| !inside) {
            return side_count;
        }
    }
}

#[test]
fn test_count_sides() {
    let region = HashSet::from([Coord::new(1, 0)]);
    assert_eq!(count_sides(&region), 4);
}

// .XX
// XXX
// .XX
#[test]
fn test_count_sides2() {
    let region = HashSet::from([
        Coord::new(1, 0),
        Coord::new(2, 0),
        Coord::new(0, 1),
        Coord::new(1, 1),
        Coord::new(2, 1),
        Coord::new(1, 2),
        Coord::new(2, 2),
    ]);
    assert_eq!(count_sides(&region), 8);
}
