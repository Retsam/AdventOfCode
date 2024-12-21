use crate::coord::Coord;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Dir8 {
    U,
    D,
    L,
    R,
    UL,
    UR,
    DL,
    DR,
}

pub const DIRS8: [Dir8; 8] = const {
    use Dir8::*;
    [U, UR, R, DR, D, DL, L, UL]
};

pub trait Neighbors {
    fn neighbors(&self) -> [Coord; 8];
    fn neighbors_with_dir(&self) -> [(Coord, Dir8); 8];
}
impl Neighbors for Coord {
    fn neighbors(&self) -> [Coord; 8] {
        DIRS8.map(|d| self.mv(d))
    }
    fn neighbors_with_dir(&self) -> [(Coord, Dir8); 8] {
        DIRS8.map(|d| (self.mv(d), d))
    }
}

impl From<Dir8> for Coord {
    fn from(value: Dir8) -> Self {
        use Dir8::*;
        let dx = match value {
            U | D => 0,
            L | UL | DL => -1,
            R | UR | DR => 1,
        };
        let dy = match value {
            L | R => 0,
            U | UL | UR => -1,
            D | DL | DR => 1,
        };
        Coord::new(dx, dy)
    }
}
