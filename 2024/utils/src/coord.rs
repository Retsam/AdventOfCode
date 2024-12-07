use crate::dir::Dir;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Coord {
    pub x: i64,
    pub y: i64,
}
impl Coord {
    pub fn mv(&self, dir: Dir) -> Coord {
        use Dir::*;
        Coord {
            x: self.x
                + match dir {
                    L => -1,
                    R => 1,
                    _ => 0,
                },
            y: self.y
                + match dir {
                    U => -1,
                    D => 1,
                    _ => 0,
                },
        }
    }
}
