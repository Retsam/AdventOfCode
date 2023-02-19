use std::{
    collections::HashSet,
    io::{self, Read},
};

#[derive(Debug, Copy, Clone)]
enum Dir {
    L,
    R,
    U,
    D,
}

impl TryFrom<&str> for Dir {
    type Error = String;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "L" => Ok(Dir::L),
            "R" => Ok(Dir::R),
            "U" => Ok(Dir::U),
            "D" => Ok(Dir::D),
            _ => Err("Foo".to_string()),
        }
    }
}
struct Move(Dir, u8);

impl TryFrom<&str> for Move {
    type Error = String;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut moves = value.split(' ');
        let dir: Dir = moves.next().ok_or("Expected move")?.try_into()?;
        let dist = moves
            .next()
            .ok_or("Expected dist")?
            .parse::<u8>()
            .map_err(|_| "Failed to parse distance")?;
        Ok(Move(dir, dist))
    }
}

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    Ok(buffer)
}

type Pos = (i32, i32);
fn update(pos: Pos, dir: Dir) -> Pos {
    let (x, y) = pos;
    match dir {
        Dir::L => (x + 1, y),
        Dir::R => (x - 1, y),
        Dir::U => (x, y - 1),
        Dir::D => (x, y + 1),
    }
}

fn follow(head: &Pos, tail: Pos) -> Pos {
    let ((hx, hy), (tx, ty)) = (head, tail);
    let dx = hx - tx;
    let dy = hy - ty;
    if dx.abs() == 2 || dy.abs() == 2 {
        (tx + dx.signum(), ty + dy.signum())
    } else {
        tail
    }
}

fn solve(moves: &Vec<Move>, knot_count: usize) -> usize {
    let mut knots = vec![(0, 0); knot_count];

    let mut tail_set = HashSet::<Pos>::new();

    for &Move(dir, dist) in moves {
        for _ in 0..dist {
            knots[0] = update(knots[0], dir);
            let mut head = knots[0];
            for knot in knots.iter_mut().skip(1) {
                *knot = follow(&head, *knot);
                head = *knot;
            }
            tail_set.insert(head);
        }
    }
    tail_set.len()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = read_input()?;

    let moves = input
        .lines()
        .map(TryInto::<Move>::try_into)
        .collect::<Result<Vec<_>, _>>()?;

    println!(
        "Part 1: {}\nPart 2: {}",
        solve(&moves, 2),
        solve(&moves, 10)
    );

    Ok(())
}
