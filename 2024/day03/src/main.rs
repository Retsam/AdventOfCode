use std::{
    io::{self, Read},
    iter::Peekable,
    str::Chars,
};

#[derive(PartialEq)]
enum Part {
    One,
    Two,
}
fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    println!("Part 1: {}", Scanner::new(&buffer).parse(Part::One));
    println!("Part 2: {}", Scanner::new(&buffer).parse(Part::Two));

    Ok(())
}

struct Scanner<'a> {
    chars: Peekable<Chars<'a>>,
}
impl<'a> Scanner<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().peekable(),
        }
    }
    fn parse(&mut self, part: Part) -> u64 {
        let mut sum: u64 = 0;
        // Part 2 only, controls whether mul operations are on
        let mut is_on = true;
        while let Some(c) = self.chars.next() {
            if c == 'm' && is_on {
                sum += self.try_parse_mul().unwrap_or(0);
            }
            if part == Part::One {
                continue;
            }
            if c == 'd' {
                is_on = self.try_parse_do_or_dont().unwrap_or(is_on);
            }
        }
        sum
    }
    fn try_parse_mul(&mut self) -> Option<u64> {
        self.match_str("ul(")?;
        let a = self.match_nums()?;
        self.match_char(',')?;
        let b = self.match_nums()?;
        self.match_char(')')?;
        Some(a * b)
    }
    fn try_parse_do_or_dont(&mut self) -> Option<bool> {
        self.match_str("on't()")
            .map(|_| false)
            .or_else(|| self.match_str("o()").map(|_| true))
    }
    // Consumes the next char if it's c, otherwise does nothing
    fn match_char(&mut self, c: char) -> Option<()> {
        let next = *self.chars.peek()?;
        (next == c).then(|| {
            self.chars.next();
        })
    }

    // Consumes chars trying to match the target
    /// Note: DOES consume as much as matched (could avoid this but it's not necessary for this problem)
    fn match_str(&mut self, target: &str) -> Option<()> {
        target
            .chars()
            .all(|c| self.match_char(c).is_some())
            .then_some(())
    }
    // parses up to three digits as a number
    fn match_nums(&mut self) -> Option<u64> {
        let mut nums = String::new();
        while nums.len() < 3 {
            if let Some(&c) = self.chars.peek() {
                if c.is_ascii_digit() {
                    nums.push(c);
                    self.chars.next();
                    continue;
                }
            }
            break;
        }
        if nums.is_empty() {
            None
        } else {
            nums.parse().ok()
        }
    }
}
