use crate::coord::Coord;

#[derive(Debug)]
pub struct Bounds {
    pub min_x: i64,
    pub min_y: i64,
    pub w: i64,
    pub h: i64,
}
impl Bounds {
    pub fn new(w: i64, h: i64) -> Self {
        Self {
            min_x: 0,
            min_y: 0,
            w,
            h,
        }
    }
    pub fn from_vec<T>(stuff: &[Vec<T>]) -> Self {
        let w = stuff[0].len() as i64;
        let h = stuff.len() as i64;
        Bounds::new(w, h)
    }
    fn range_x(&self) -> std::ops::Range<i64> {
        self.min_x..(self.min_x + self.w)
    }
    fn range_y(&self) -> std::ops::Range<i64> {
        self.min_y..(self.min_y + self.h)
    }

    pub fn iter(&self) -> impl Iterator<Item = Coord> + '_ {
        self.range_x()
            .flat_map(|x| self.range_y().map(move |y| Coord { x, y }))
    }
    pub fn in_bounds(&self, coord: Coord) -> bool {
        coord.x >= self.min_x
            && coord.x < self.min_x + self.w
            && coord.y >= self.min_y
            && coord.y < self.min_y + self.h
    }

    pub fn debug<F, S: ToString>(&self, cb: F)
    where
        F: Fn(Coord) -> S,
    {
        let mut res = String::new();
        for y in self.range_y() {
            for x in self.range_x() {
                let coord = Coord { x, y };
                let char = cb(coord).to_string();
                res.push_str(&char);
            }
            res.push('\n');
        }
        println!("{res}");
    }
}
