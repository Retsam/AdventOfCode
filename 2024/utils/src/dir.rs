use crate::coord::Coord;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Dir {
    U,
    D,
    L,
    R,
}

pub const DIRS: [Dir; 4] = [Dir::U, Dir::D, Dir::L, Dir::R];

impl Dir {
    pub fn cw(&self) -> Dir {
        use Dir::*;
        match self {
            L => U,
            D => L,
            R => D,
            U => R,
        }
    }
    pub fn ccw(&self) -> Dir {
        use Dir::*;
        match self {
            L => D,
            D => R,
            R => U,
            U => L,
        }
    }
    pub fn neighbors(coord: &Coord) -> [Coord; 4] {
        DIRS.map(|d| coord.mv(d))
    }
}

impl From<Dir> for Coord {
    fn from(value: Dir) -> Self {
        match value {
            Dir::U => Coord::new(0, -1),
            Dir::D => Coord::new(0, 1),
            Dir::L => Coord::new(-1, 0),
            Dir::R => Coord::new(1, 0),
        }
    }
}
