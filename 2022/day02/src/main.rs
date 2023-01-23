use std::io::{self, Read};

#[derive(Debug)]
enum XYZ {
    X,
    Y,
    Z,
}
impl TryFrom<char> for XYZ {
    type Error = ();
    fn try_from(s: char) -> Result<Self, ()> {
        match s {
            'X' => Ok(XYZ::X),
            'Y' => Ok(XYZ::Y),
            'Z' => Ok(XYZ::Z),
            _ => Err(()),
        }
    }
}
#[derive(Debug)]
enum ABC {
    A,
    B,
    C,
}
impl TryFrom<char> for ABC {
    type Error = ();
    fn try_from(s: char) -> Result<Self, Self::Error> {
        match s {
            'A' => Ok(ABC::A),
            'B' => Ok(ABC::B),
            'C' => Ok(ABC::C),
            _ => Err(()),
        }
    }
}
type Round = (ABC, XYZ);

fn part1((a, x): Round) -> i32 {
    match a {
        ABC::A => match x {
            XYZ::X => 1 + 3,
            XYZ::Y => 2 + 6,
            XYZ::Z => 3,
        },
        ABC::B => match x {
            XYZ::X => 1,
            XYZ::Y => 2 + 3,
            XYZ::Z => 3 + 6,
        },
        ABC::C => match x {
            XYZ::X => 1 + 6,
            XYZ::Y => 2,
            XYZ::Z => 3 + 3,
        },
    }
}

fn part2((a, x): Round) -> i32 {
    match a {
        ABC::A => match x {
            XYZ::X => 3,
            XYZ::Y => 1 + 3,
            XYZ::Z => 2 + 6,
        },
        ABC::B => match x {
            XYZ::X => 1,
            XYZ::Y => 2 + 3,
            XYZ::Z => 3 + 6,
        },
        ABC::C => match x {
            XYZ::X => 2,
            XYZ::Y => 3 + 3,
            XYZ::Z => 1 + 6,
        },
    }
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    fn parse_line(l: &str) -> Result<Round, ()> {
        let mut chars = l.chars();

        let (Some(a), Some(' '), Some(b)) = (chars.next(), chars.next(), chars.next()) else {
            return Err(());
        };
        Ok((a.try_into()?, b.try_into()?))
    }

    let rounds = buffer.lines().map(parse_line).map(|o| o.unwrap());

    let p1: i32 = rounds.clone().map(part1).sum();
    let p2: i32 = rounds.map(part2).sum();
    println!("Part 1: {}\nPart 2: {}", p1, p2);

    Ok(())
}
