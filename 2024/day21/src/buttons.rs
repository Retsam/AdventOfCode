use std::fmt::Display;

use utils::{coord::Coord, dir::Dir};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum NumButton {
    A,
    Zero,
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
}
impl From<NumButton> for Coord {
    fn from(value: NumButton) -> Self {
        match value {
            NumButton::Seven => Coord { x: 0, y: 0 },
            NumButton::Eight => Coord { x: 1, y: 0 },
            NumButton::Nine => Coord { x: 2, y: 0 },

            NumButton::Four => Coord { x: 0, y: 1 },
            NumButton::Five => Coord { x: 1, y: 1 },
            NumButton::Six => Coord { x: 2, y: 1 },

            NumButton::One => Coord { x: 0, y: 2 },
            NumButton::Two => Coord { x: 1, y: 2 },
            NumButton::Three => Coord { x: 2, y: 2 },

            NumButton::Zero => Coord { x: 1, y: 3 },
            NumButton::A => Coord { x: 2, y: 3 },
        }
    }
}
impl TryFrom<Coord> for NumButton {
    type Error = String;
    fn try_from(value: Coord) -> Result<NumButton, Self::Error> {
        match value {
            Coord { x: 0, y: 0 } => Ok(Self::Seven),
            Coord { x: 1, y: 0 } => Ok(Self::Eight),
            Coord { x: 2, y: 0 } => Ok(Self::Nine),

            Coord { x: 0, y: 1 } => Ok(Self::Four),
            Coord { x: 1, y: 1 } => Ok(Self::Five),
            Coord { x: 2, y: 1 } => Ok(Self::Six),

            Coord { x: 0, y: 2 } => Ok(Self::One),
            Coord { x: 1, y: 2 } => Ok(Self::Two),
            Coord { x: 2, y: 2 } => Ok(Self::Three),

            Coord { x: 1, y: 3 } => Ok(Self::Zero),
            Coord { x: 2, y: 3 } => Ok(Self::A),
            _ => Err(format!("Invalid Coord for NumButton: {value:?}")),
        }
    }
}

pub const NUMS: [NumButton; 11] = const {
    use NumButton::*;
    [
        A, Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine,
    ]
};

impl TryFrom<char> for NumButton {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '0' => Ok(Self::Zero),
            '1' => Ok(Self::One),
            '2' => Ok(Self::Two),
            '3' => Ok(Self::Three),
            '4' => Ok(Self::Four),
            '5' => Ok(Self::Five),
            '6' => Ok(Self::Six),
            '7' => Ok(Self::Seven),
            '8' => Ok(Self::Eight),
            '9' => Ok(Self::Nine),
            'A' => Ok(Self::A),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum DirButton {
    Up,
    Down,
    Left,
    Right,
    A,
}

pub const DIR_BUTTONS: [DirButton; 5] = const {
    use DirButton::*;
    [Up, Down, Left, Right, A]
};

impl From<Dir> for DirButton {
    fn from(value: Dir) -> DirButton {
        match value {
            Dir::U => Self::Up,
            Dir::D => Self::Down,
            Dir::L => Self::Left,
            Dir::R => Self::Right,
        }
    }
}

impl From<DirButton> for Coord {
    fn from(value: DirButton) -> Coord {
        use DirButton::*;
        match value {
            Up => Coord { x: 1, y: 0 },
            Down => Coord { x: 1, y: 1 },
            Left => Coord { x: 0, y: 1 },
            Right => Coord { x: 2, y: 1 },
            A => Coord { x: 2, y: 0 },
        }
    }
}
impl TryFrom<Coord> for DirButton {
    type Error = String;
    fn try_from(value: Coord) -> Result<DirButton, Self::Error> {
        match value {
            Coord { x: 1, y: 0 } => Ok(Self::Up),
            Coord { x: 1, y: 1 } => Ok(Self::Down),
            Coord { x: 0, y: 1 } => Ok(Self::Left),
            Coord { x: 2, y: 1 } => Ok(Self::Right),
            Coord { x: 2, y: 0 } => Ok(Self::A),
            x => Err(format!("Invalid coord for DirButton {x}")),
        }
    }
}

impl Display for DirButton {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Up => "U",
                Self::Down => "D",
                Self::Left => "L",
                Self::Right => "R",
                Self::A => "A",
            }
        )
    }
}

impl Display for NumButton {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::A => "A",
                Self::Zero => "0",
                Self::One => "1",
                Self::Two => "2",
                Self::Three => "3",
                Self::Four => "4",
                Self::Five => "5",
                Self::Six => "6",
                Self::Seven => "7",
                Self::Eight => "8",
                Self::Nine => "9",
            }
        )
    }
}
