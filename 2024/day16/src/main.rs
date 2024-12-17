use std::collections::{BinaryHeap, HashMap, HashSet};
use std::error;
use std::io::{self, Read};

use colored::Colorize;
use itertools::Itertools;
use utils::coord::Coord;
use utils::dir::Dir;

type Grid = utils::grid::Grid<char>;

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let grid = Grid::parse(&buf);
    let start = grid.find_coord(|c| *c == 'S').expect("No start");
    let goal = grid.find_coord(|c| *c == 'E').expect("No goal");

    let (p1, p2) = search(&grid, start, goal);

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Node {
    score: u64,
    coord: Coord,
    dir: Dir,
}
#[derive(Debug, PartialEq, Eq, Clone)]
struct SearchEntry {
    node: Node,
    path: Vec<Coord>,
}
impl SearchEntry {
    fn new_entry(&self, node: Node) -> Self {
        let mut path = self.path.clone();
        path.push(node.coord);
        SearchEntry { node, path }
    }
    fn forward(&self) -> Self {
        self.new_entry(Node {
            score: self.node.score + 1,
            coord: self.node.coord.mv(self.node.dir),
            dir: self.node.dir,
        })
    }
    fn turn(&self, dir: Dir) -> Self {
        self.new_entry(Node {
            score: self.node.score + 1001,
            coord: self.node.coord.mv(dir),
            dir,
        })
    }
}
impl PartialOrd for SearchEntry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for SearchEntry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.node.score.cmp(&other.node.score).reverse()
    }
}

type VisitedMap = HashMap<(Coord, Dir), Node>;
fn search(grid: &Grid, start: Coord, goal: Coord) -> (u64, usize) {
    let start_dir = Dir::R;
    let search_start = SearchEntry {
        node: Node {
            score: 0,
            coord: start,
            dir: start_dir,
        },
        path: vec![start],
    };
    let mut visited = VisitedMap::from_iter([((start, start_dir), search_start.node)]);

    let mut queue = BinaryHeap::<SearchEntry>::new();
    queue.push(search_start);

    let mut best_solution = None;
    let mut solution_nodes = HashSet::<Coord>::new();

    while let Some(entry) = queue.pop() {
        let neighbors = [
            entry.forward(),
            entry.turn(entry.node.dir.ccw()),
            entry.turn(entry.node.dir.cw()),
        ];
        for neighbor in neighbors
            .into_iter()
            .filter(|entry| matches!(grid.get(entry.node.coord).cloned(), Some('.') | Some('E')))
            .filter(|entry| {
                visited
                    .get(&(entry.node.coord, entry.node.dir))
                    .map(|&prev| prev.score >= entry.node.score)
                    .unwrap_or(true)
            })
            .collect_vec()
        {
            if neighbor.node.coord == goal {
                if let Some(best) = best_solution {
                    if neighbor.node.score > best {
                        continue;
                    }
                }
                best_solution = Some(neighbor.node.score);
                // println!("Found goal at {neighbor:?}");
                solution_nodes.extend(neighbor.path.iter());
                continue;
            }
            if let std::collections::hash_map::Entry::Vacant(e) =
                visited.entry((neighbor.node.coord, neighbor.node.dir))
            {
                e.insert(neighbor.node);
            }
            queue.push(neighbor);
        }
    }

    (best_solution.expect("No path found"), solution_nodes.len())
}

#[allow(unused)]
fn print_path(grid: &Grid, entry: &SearchEntry) {
    grid.debug_map(|(c, coord)| {
        if entry.path.contains(&coord) {
            "*".bright_red().to_string()
        } else {
            c.to_string()
        }
    });
}
