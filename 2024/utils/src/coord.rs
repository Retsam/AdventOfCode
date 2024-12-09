#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Coord {
    pub x: i64,
    pub y: i64,
}
impl Coord {
    pub fn new(x: i64, y: i64) -> Self {
        Coord { x, y }
    }
    pub fn mv<T: Into<Coord>>(&self, c2: T) -> Coord {
        let c2: Coord = c2.into();
        Coord {
            x: self.x + c2.x,
            y: self.y + c2.y,
        }
    }
}

impl From<(i64, i64)> for Coord {
    fn from((x, y): (i64, i64)) -> Self {
        Coord { x, y }
    }
}

#[cfg(test)]
mod test {
    use crate::{coord::Coord, dir::Dir};

    #[test]
    fn test_mv() {
        let coord = Coord::new(5, 5);
        assert_eq!(coord.mv(Dir::U), Coord::new(5, 4));
        assert_eq!(coord.mv(coord), Coord::new(10, 10));
    }
}
