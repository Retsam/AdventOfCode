use std::error;
use std::io::{self, Read};

use itertools::Itertools;

// also lock w/h
const KEY_W: usize = 5;
const KEY_H: usize = 6;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct KeyLock {
    kind: Kind,
    sizes: [usize; KEY_W],
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Kind {
    Key,
    Lock,
}
struct LocksAndKeys {
    locks: Vec<KeyLock>,
    keys: Vec<KeyLock>,
}

fn parse_input(input: &str) -> LocksAndKeys {
    use Kind::*;
    let mut keys = vec![];
    let mut locks = vec![];
    for pattern in input.trim().split("\n\n") {
        let kind = if pattern.starts_with("#####") {
            Lock
        } else {
            Key
        };
        let sizes = [0, 1, 2, 3, 4].map(|x| {
            for size in 0..KEY_H {
                let y = match kind {
                    Lock => size + 1,
                    Key => KEY_H - (size + 1),
                };
                let char = pattern
                    .chars()
                    .nth(y * (KEY_W + 1) + x) // + 1 from the \n
                    .expect("char out of bounds");
                if char == '.' {
                    return size;
                }
            }
            panic!("size not found");
        });
        let key_lock = KeyLock { kind, sizes };
        match kind {
            Lock => locks.push(key_lock),
            Key => keys.push(key_lock),
        }
    }
    LocksAndKeys { locks, keys }
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let LocksAndKeys { keys, locks } = parse_input(&buf);

    let p1 = keys
        .iter()
        .cartesian_product(locks.iter())
        .filter(|(key, lock)| {
            key.sizes
                .iter()
                .zip(lock.sizes.iter())
                .all(|(k, l)| k + l < KEY_H)
        })
        .count();

    println!("Part 1: {p1}\nPart 2: Merry Christmas");

    Ok(())
}
