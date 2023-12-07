use std::{
    cell::Cell,
    collections::HashMap,
    fmt::Display,
    io::{self, Read},
    str::FromStr,
};

#[derive(Debug, Clone)]
enum Tile {
    Digit(char),
    Part,
    Gear,
}
#[derive(Debug)]
struct Grid {
    max_x: usize,
    max_y: usize,
    map: HashMap<(usize, usize), Tile>,
}

impl FromStr for Grid {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Using cells so they can be mutated from inside the flat_map
        let max_x = &Cell::new(0);
        let max_y = &Cell::new(0);

        let map = s
            .lines()
            .enumerate()
            .flat_map(|(y, line)| {
                line.chars().enumerate().flat_map(move |(x, char)| {
                    match char {
                        '.' => None,
                        '0'..='9' => Some(Tile::Digit(char)),
                        '*' => Some(Tile::Gear),
                        _ => Some(Tile::Part),
                    }
                    .map(|tile| {
                        max_x.set(max_x.get().max(x));
                        max_y.set(max_y.get().max(y));

                        ((x, y), tile)
                    })
                })
            })
            .collect::<HashMap<_, _>>();

        Ok(Grid {
            map,
            max_x: max_x.get(),
            max_y: max_y.get(),
        })
    }
}

impl Grid {
    fn cursor(&self) -> Cursor {
        Cursor {
            x: 0,
            y: 0,
            min_x: 0,
            max_x: self.max_x,
            max_y: self.max_y,
        }
    }
    fn read(&self, cursor: &Cursor) -> Option<&Tile> {
        self.map.get(&(cursor.x, cursor.y))
    }
}

struct Cursor {
    x: usize,
    y: usize,
    min_x: usize,
    max_x: usize,
    max_y: usize,
}
impl Cursor {
    fn advance(&mut self) {
        if self.x == self.max_x {
            self.x = self.min_x;
            self.y += 1;
        } else {
            self.x += 1
        }
    }
    fn is_at_end(&self) -> bool {
        self.y > self.max_y
    }
}
impl Display for Cursor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)?;
        Ok(())
    }
}

fn main() -> io::Result<()> {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf)?;
    let grid: Grid = buf.parse().unwrap();

    let mut cursor = grid.cursor();

    let mut part1: u32 = 0;
    // Part 2 map
    let mut gear_map = HashMap::<(usize, usize), Vec<u32>>::new();

    while !cursor.is_at_end() {
        if let Some(Tile::Digit(d)) = grid.read(&cursor) {
            // When we find a digit, read to the end of the line
            let mut digits = vec![*d];
            let x = cursor.x;
            let y = cursor.y;
            while cursor.y == y {
                cursor.advance();
                if let Some(Tile::Digit(d)) = grid.read(&cursor) {
                    digits.push(*d);
                } else {
                    break;
                }
            }

            let value = digits.iter().collect::<String>().parse::<u32>().unwrap();

            let mut area_cursor = Cursor {
                x: x.max(1) - 1,
                y: y.max(1) - 1,
                min_x: x.max(1) - 1,
                max_x: x + digits.len(),
                max_y: y + 1,
            };

            let mut found_part = false;
            while !area_cursor.is_at_end() {
                match grid.read(&area_cursor) {
                    Some(Tile::Gear) => {
                        found_part = true;
                        gear_map
                            .entry((area_cursor.x, area_cursor.y))
                            .or_default()
                            .push(value);
                    }
                    Some(Tile::Part) => found_part = true,
                    _ => {}
                }
                area_cursor.advance();
            }
            if found_part {
                part1 += value;
            }
        } else {
            cursor.advance();
        }
    }
    let part2: u32 = gear_map
        .into_values()
        .filter(|vals| vals.len() == 2)
        .map(|vals| vals.into_iter().product::<u32>())
        .sum();
    println!("{} {}", part1, part2);

    Ok(())
}
