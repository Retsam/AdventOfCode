use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::cmp::{min, max};
use core::fmt;

pub type Grid<T> = HashMap<Coord, T>;

pub trait GridCode {
    fn grid_code(&self) -> char;
}
impl GridCode for char {
    fn grid_code(&self) -> char { *self }
}

struct GridBounds {
    min_x: i32,
    max_x: i32,
    min_y: i32,
    max_y: i32,
}

fn boundaries<T>(grid: &HashMap<Coord, T>) -> GridBounds {
    grid.keys().fold(GridBounds {
        min_x: 0, min_y: 0,
        max_x: 0, max_y: 0,
    }, |mut bounds, coord| {
        bounds.min_x = min(bounds.min_x, coord.x);
        bounds.min_y = min(bounds.min_y, coord.y);
        bounds.max_x = max(bounds.max_x, coord.x);
        bounds.max_y = max(bounds.max_y, coord.y);
        bounds
    })
}

pub fn print<T: GridCode>(grid: &HashMap<Coord, T>, default: &T) -> String {
    print_with_overrides(grid, &HashMap::new(), default)
}
pub fn print_with_overrides<T: GridCode>(grid: &HashMap<Coord, T>, overrides: &HashMap<Coord, char>, default: &T) -> String {
    let bounds = boundaries(grid);
    let mut out = String::new();

    for y in bounds.min_y..=bounds.max_y {
        for x in bounds.min_x..=bounds.max_x {
            let coord = &Coord::new(x, y);
            out.push(overrides.get(coord).cloned().unwrap_or_else(|| {
                grid.get(coord).unwrap_or(default).grid_code()
            }));
        }
        out.push('\n');
    }
    out
}

pub fn find_path<T: GridCode, F>(grid: &Grid<T>, from: Coord, to: Coord, is_open: F) -> Option<Vec<Coord>>
    where F: Fn(&T) -> bool {
    let mut search_queue = VecDeque::new();
    let mut searched = HashSet::new();
    search_queue.push_back((vec!(), from));
    loop {
        if let Some((path, node)) = search_queue.pop_front() {
            searched.insert(node); // Technically could be done when added to queue
            let neighbors = node.neighbors();
            let mut new_path = path.clone();
            new_path.push(node);
            if neighbors.contains(&to) {
                new_path.push(to);
                new_path.drain(..1); //Remove first item
                break Some(new_path);
            }

            let new_search: Vec<_> = neighbors.into_iter()
                .filter(|c| grid.get(c).map(|x| is_open(x)).unwrap_or(false))
                .filter(|c| !searched.contains(c))
                .map(|c| (new_path.clone(), *c))
                .collect();
            search_queue.append(&mut VecDeque::from(new_search));
        } else {
            break None;
        }

    }
}

#[derive(Debug, Clone, Copy)]
pub enum Dir { Up, Down, Left, Right }
use Dir::{*};

impl Dir {
    pub fn cw(self) -> Dir {
        match self {
            Up => Right,
            Right => Down,
            Down => Left,
            Left => Up,
        }
    }
    pub fn ccw(self) -> Dir {
        match self {
            Up => Left,
            Right => Up,
            Down => Right,
            Left => Down,
        }
    }
    pub fn rev(self) -> Dir {
        match self {
            Up => Down,
            Down => Up,
            Right => Left,
            Left => Right,
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct Coord {
    pub x: i32,
    pub y: i32,
}
impl Coord {
    pub fn new(x: i32, y: i32) -> Coord {
        Coord { x, y }
    }
    pub fn go(&self, dir: Dir) -> Coord {
        let x = self.x;
        let y = self.y;
        match dir {
            Up    => Coord { x, y: y - 1 },
            Down  => Coord { x, y: y + 1 },
            Left  => Coord { x: x - 1, y },
            Right => Coord { x: x + 1, y },
        }
    }
    pub fn neighbors(&self) -> [Coord; 4] {
        [self.go(Up), self.go(Right), self.go(Down), self.go(Left)]
    }
}
impl fmt::Display for Coord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

#[cfg(test)]
mod tests {
    use super::{*};
    #[test]
    fn find_path_test() {
        let path = vec!(Coord::new(0, 1), Coord::new(1,1), Coord::new(2,1));
        let grid: Grid<char> = path.iter().cloned().map(|c| (c, '.')).collect();
        assert_eq!(path,
            find_path(&grid, Coord::new(0, 0),  Coord::new(2,1), |_| true).unwrap()
        )
    }
}
