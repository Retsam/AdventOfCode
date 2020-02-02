use std::collections::HashMap;
use std::cmp::{min, max};

pub trait GridCode {
    fn grid_code(&self) -> char;
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
    let bounds = boundaries(grid);
    let mut out = String::new();

    for y in bounds.min_y..=bounds.max_y {
        for x in bounds.min_x..=bounds.max_x {
            let tile = grid.get(&Coord::new(x, y)).unwrap_or(default);
            out.push(tile.grid_code())
        }
        out.push('\n');
    }
    out
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
}
