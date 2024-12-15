use crate::{bounds::Bounds, coord::Coord};

pub struct Grid<T> {
    pub data: Vec<Vec<T>>,
    pub bounds: Bounds,
}

impl Grid<char> {
    pub fn parse(str: &str) -> Self {
        Grid::new(
            str.trim()
                .lines()
                .map(|line| line.chars().collect())
                .collect(),
        )
    }
}

impl<T> Grid<T> {
    pub fn new(grid: Vec<Vec<T>>) -> Self {
        let bounds = Bounds::from_vec(&grid);
        Self { data: grid, bounds }
    }

    pub fn parse_with(str: &str, f: impl Fn(char) -> T) -> Self {
        Grid::new(
            str.trim()
                .lines()
                .map(|line| line.chars().map(&f).collect())
                .collect(),
        )
    }

    pub fn map<U>(self, f: impl Fn(T) -> U) -> Grid<U> {
        Grid {
            data: self
                .data
                .into_iter()
                .map(|row| row.into_iter().map(&f).collect())
                .collect(),
            bounds: self.bounds,
        }
    }

    pub fn get(&self, c: Coord) -> Option<&T> {
        self.data
            .get(c.y as usize)
            .and_then(|row| row.get(c.x as usize))
    }
    pub fn set(&mut self, c: Coord, data: T) -> bool {
        if !self.bounds.in_bounds(c) {
            return false;
        }
        self.data[c.y as usize][c.x as usize] = data;
        true
    }
    pub fn iter(&self) -> impl Iterator<Item = &T> + '_ {
        self.bounds.iter().map(|c| self.get(c).unwrap())
    }
    pub fn iter_with_coord(&self) -> impl Iterator<Item = (&T, Coord)> + '_ {
        self.bounds.iter().map(|c| (self.get(c).unwrap(), c))
    }
    pub fn debug_map(&self, f: impl Fn(&T) -> String) {
        self.bounds.debug(|c| f(self.get(c).unwrap()))
    }
}

impl<T: ToString> Grid<T> {
    pub fn debug(&self) {
        self.bounds.debug(|c| self.get(c).unwrap().to_string());
    }
}

#[test]
fn debug() {
    // Not really run-time test since capturing stdio is annoying
    let grid = Grid::new(vec![
        vec!["a", "b", "c"],
        vec!["d", "e", "f"],
        vec!["g", "h", "i"],
    ]);
    grid.debug();
    grid.debug_map(|x| x.to_uppercase());
}
