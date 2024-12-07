#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Dir {
    U,
    D,
    L,
    R,
}

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
}
