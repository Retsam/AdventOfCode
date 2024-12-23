use std::error;
use std::io::{self, Read};

use colored::Colorize;
use itertools::Itertools;
use utils::coord::Coord;
use utils::dir::Dir;
use utils::grid::Grid;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Tile {
    Empty,
    Wall,
    Box,
    Robot,
    BoxL,
    BoxR,
}
impl Tile {
    fn parse(c: char) -> Option<Self> {
        match c {
            '.' => Some(Self::Empty),
            '#' => Some(Self::Wall),
            'O' => Some(Self::Box),
            '@' => Some(Self::Robot),
            _ => None,
        }
    }
    fn parse_big(c: char) -> Option<[Self; 2]> {
        match c {
            '.' => Some([Self::Empty, Self::Empty]),
            '#' => Some([Self::Wall, Self::Wall]),
            'O' => Some([Self::BoxL, Self::BoxR]),
            '@' => Some([Self::Robot, Self::Empty]),
            _ => None,
        }
    }
    fn is_big_box(&self) -> bool {
        matches!(self, Self::BoxL | Self::BoxR)
    }
    fn reverse(&self) -> Self {
        match self {
            Self::BoxL => Self::BoxR,
            Self::BoxR => Self::BoxL,
            x => panic!("Unexpected {x:?}"),
        }
    }
}

#[allow(clippy::to_string_trait_impl)]
impl ToString for Tile {
    fn to_string(&self) -> String {
        match self {
            Self::Empty => ".".into(),
            Self::Wall => "#".into(),
            Self::Box => "O".into(),
            Self::Robot => "@".red().to_string(),
            Self::BoxL => "[".to_string(),
            Self::BoxR => "]".to_string(),
        }
    }
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;
    _main(buf)
}

fn _main(buf: String) -> Result<(), Box<dyn error::Error>> {
    let (map_s, moves_s) = buf.trim().splitn(2, "\n\n").collect_tuple().unwrap();
    let moves = moves_s
        .lines()
        .flat_map(|line| line.chars())
        .map(|c| match c {
            '^' => Dir::U,
            '>' => Dir::R,
            'v' => Dir::D,
            '<' => Dir::L,
            x => panic!("Unexpected {x}"),
        })
        .collect_vec();

    let p1 = solve(
        Grid::parse_with(map_s, |c| {
            Tile::parse(c).unwrap_or_else(|| panic!("Unexpected {c}"))
        }),
        &moves,
    );

    let part2_data = map_s
        .trim()
        .lines()
        .map(|line| {
            line.chars()
                .flat_map(|c| Tile::parse_big(c).unwrap_or_else(|| panic!("Unexpected {c}")))
                .collect_vec()
        })
        .collect_vec();
    let p2 = solve(Grid::new(part2_data), &moves);

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}

fn solve(mut grid: Grid<Tile>, moves: &[Dir]) -> i64 {
    let mut coord = grid.find_coord(|t| *t == Tile::Robot).unwrap();

    for &dir in moves.iter() {
        do_move(&mut grid, &mut coord, dir);
    }

    grid.iter_with_coord()
        .filter(|(&t, _)| matches!(t, Tile::Box | Tile::BoxL))
        .map(|(_, c)| 100 * c.y + c.x)
        .sum::<i64>()
}

fn try_push(grid: &mut Grid<Tile>, from: Coord, dir: Dir) -> bool {
    use Tile::*;

    let tile = *grid.expect(from);
    // Part 1
    if tile == Box {
        let mut dest = from.mv(dir);
        while *grid.expect(dest) == Box {
            dest = dest.mv(dir);
        }
        if *grid.expect(dest) == Empty {
            grid.set(dest, Box);
            grid.set(from, Empty);
            return true;
        } else {
            return false;
        }
    }
    assert!(tile == BoxL || tile == BoxR);
    match dir {
        // Part 2 - horizontal
        Dir::L | Dir::R => {
            let mut dest = from.mv(dir);
            let mut box_coords = vec![];
            while grid.expect(dest).is_big_box() {
                box_coords.push(dest);
                dest = dest.mv(dir);
            }
            if *grid.expect(dest) == Empty {
                grid.set(dest, tile.reverse());
                grid.set(from, Empty);
                for c in box_coords {
                    grid.set(c, grid.expect(c).reverse());
                }
                true
            } else {
                false
            }
        }
        // Part 2 - vertical
        _ => try_vert_push(grid, from, dir)
            .map(|push| push.exec(grid))
            .is_some(),
    }
}
struct VertPush {
    start: (Coord, Coord),
    dest: (Coord, Coord),
    depends_on: Vec<VertPush>,
}
impl VertPush {
    fn exec(self, grid: &mut Grid<Tile>) {
        for push in self.depends_on {
            push.exec(grid);
        }
        if grid.get(self.start.0) == Some(&Tile::Empty)
            || grid.get(self.start.1) == Some(&Tile::Empty)
        {
            // Might have already been moved, e.g. a pyramid formation
            return;
        }
        grid.set(self.dest.0, *grid.expect(self.start.0));
        grid.set(self.dest.1, *grid.expect(self.start.1));
        grid.set(self.start.0, Tile::Empty);
        grid.set(self.start.1, Tile::Empty);
    }
}
fn try_vert_push(grid: &Grid<Tile>, from: Coord, dir: Dir) -> Option<VertPush> {
    let tile = *grid.expect(from);
    if tile == Tile::Wall {
        return None;
    }
    let pair_dir = match tile {
        Tile::BoxL => Dir::R,
        Tile::BoxR => Dir::L,
        _ => panic!("Unexpected {tile:?}"),
    };
    let pair_coord = from.mv(pair_dir);
    assert!(*grid.expect(pair_coord) == tile.reverse());

    let mut dest = from.mv(dir);
    while *grid.expect(dest) == tile {
        dest = dest.mv(dir);
    }

    let dests = [dest, dest.mv(pair_dir)];
    if dests.iter().any(|c| *grid.expect(*c) == Tile::Wall) {
        return None;
    }
    let depends_on = dests
        .iter()
        .filter(|c| *grid.expect(**c) != Tile::Empty)
        .map(|c| try_vert_push(grid, *c, dir))
        .collect::<Option<Vec<_>>>()?;
    Some(VertPush {
        start: (from, pair_coord),
        dest: (dests[0], dests[1]),
        depends_on,
    })
}

fn do_move(grid: &mut Grid<Tile>, coord: &mut Coord, dir: Dir) {
    use Tile::*;
    let next = coord.mv(dir);
    let tile = *grid.expect(next);
    if tile == Wall {
        return;
    }
    if tile != Empty && !try_push(grid, next, dir) {
        return;
    }
    grid.set(*coord, Empty);
    *coord = next;
    grid.set(*coord, Robot);
}

#[test]
fn push_horiz() {
    let input = r#"
##########
#........#
#.O.O.O.@#
#........#
##########

<<<<<<<<<<<"#;
    _main(input.to_string()).unwrap();
}
#[test]
fn push_vert_1() {
    let input = r#"
#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^"#;
    _main(input.to_string()).unwrap();
}

#[test]
fn push_vert_2() {
    let input = r#"
#######
#.....#
#.....#
#..O..#
#..O..#
#..@..#
#######

^^"#;
    _main(input.to_string()).unwrap();
}

#[test]
fn push_vert_bug() {
    let input = r#"
#######
#..@..#
#..O..#
#.OO..#
#.OOO.#
#.....#
#######

<<<vv>^^>>v
"#;
    _main(input.to_string()).unwrap();
}
