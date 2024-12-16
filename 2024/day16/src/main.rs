use std::collections::{BinaryHeap, HashMap, HashSet};
use std::error;
use std::io::{self, Read};

use colored::Colorize;
use itertools::{kmerge, Itertools};
use utils::coord::{self, Coord};
use utils::dir::Dir;

type Grid = utils::grid::Grid<char>;

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let (p1, p2) = (0, 0);

    let grid = Grid::parse(&buf);
    let start = grid.find_coord(|c| *c == 'S').expect("No start");
    let goal = grid.find_coord(|c| *c == 'E').expect("No goal");

    let p1 = search(&grid, start, goal);

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct SearchEntry {
    score: u64,
    coord: Coord,
    dir: Dir,
    prev: Option<Coord>,
}
impl SearchEntry {
    fn forward(&self) -> Self {
        SearchEntry {
            score: self.score + 1,
            coord: self.coord.mv(self.dir),
            dir: self.dir,
            prev: Some(self.coord),
        }
    }
    fn turn(&self, dir: Dir) -> Self {
        SearchEntry {
            score: self.score + 1001,
            coord: self.coord.mv(dir),
            dir,
            prev: Some(self.coord),
        }
    }
}
impl PartialOrd for SearchEntry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for SearchEntry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.score.cmp(&other.score).reverse()
    }
}

type VisitedMap = HashMap<Coord, SearchEntry>;
fn search(grid: &Grid, start: Coord, goal: Coord) -> u64 {
    let search_start = SearchEntry {
        score: 0,
        coord: start,
        dir: Dir::R,
        prev: None,
    };
    let mut visited = VisitedMap::from_iter([(start, search_start)]);

    let mut queue = BinaryHeap::<SearchEntry>::new();
    queue.push(search_start);

    while let Some(entry) = queue.pop() {
        let neighbors = [
            entry.forward(),
            entry.turn(entry.dir.ccw()),
            entry.turn(entry.dir.cw()),
        ];
        for neighbor in neighbors
            .into_iter()
            .filter(|entry| matches!(grid.get(entry.coord).cloned(), Some('.') | Some('E')))
            .filter(|entry| {
                visited
                    .get(&entry.coord)
                    .map(|&prev| prev.score > entry.score)
                    .unwrap_or(true)
            })
            .collect_vec()
        {
            if neighbor.coord == goal {
                print_path(grid, start, &neighbor, &visited);
                return neighbor.score;
            }
            visited.insert(neighbor.coord, neighbor);
            queue.push(neighbor);
        }
    }

    panic!("No path found")
}

#[allow(unused)]
fn print_path(grid: &Grid, start: Coord, entry: &SearchEntry, visited: &VisitedMap) {
    let mut tile = entry.prev.unwrap();
    let mut path = HashSet::<Coord>::new();
    while tile != start {
        path.insert(tile);
        tile = visited
            .get(&tile)
            .unwrap_or_else(|| panic!("No entry {tile:?}"))
            .prev
            .expect("Was None");
    }
    grid.debug_map(|(c, coord)| {
        if path.contains(&coord) {
            "*".bright_red().to_string()
        } else {
            c.to_string()
        }
    });
}
