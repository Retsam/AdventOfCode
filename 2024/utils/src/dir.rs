use crate::coord::Coord;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Dir {
    U,
    D,
    L,
    R,
}

pub const DIRS: [Dir; 4] = [Dir::U, Dir::R, Dir::D, Dir::L];

impl Dir {
    pub fn cw(&self) -> Self {
        use Dir::*;
        match self {
            L => U,
            D => L,
            R => D,
            U => R,
        }
    }
    pub fn ccw(&self) -> Self {
        use Dir::*;
        match self {
            L => D,
            D => R,
            R => U,
            U => L,
        }
    }
    pub fn rev(&self) -> Self {
        use Dir::*;
        match self {
            L => R,
            D => U,
            R => L,
            U => D,
        }
    }
    pub fn is_horiz(&self) -> bool {
        self == &Dir::L || self == &Dir::R
    }
}

pub trait Neighbors {
    fn neighbors(&self) -> [Coord; 4];
    fn neighbors_with_dir(&self) -> [(Coord, Dir); 4];
}
impl Neighbors for Coord {
    fn neighbors(&self) -> [Coord; 4] {
        DIRS.map(|d| self.mv(d))
    }
    fn neighbors_with_dir(&self) -> [(Coord, Dir); 4] {
        DIRS.map(|d| (self.mv(d), d))
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
