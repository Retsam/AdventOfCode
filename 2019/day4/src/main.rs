use std::collections::HashMap;

const RANGE_MIN: u32 = 138241;
const RANGE_MAX: u32 = 674034;

const PART_2: bool = true;

fn is_ascending(x: u32) -> bool {
    let str1 = x.to_string();
    let mut chars = str1.chars().collect::<Vec<_>>();
    chars.sort();
    chars.into_iter().collect::<String>() == str1
}

fn meets_criteria(x: u32) -> bool {
    is_ascending(x) && has_group(x)
}

// Since we know matching passwords are ascending, we don't actually need to check adjacency
fn has_group(x: u32) -> bool {
    let mut digit_map = HashMap::new();
    for c in x.to_string().chars() {
        let counter = digit_map.entry(c).or_insert(0);
        *counter += 1
    };
    digit_map.into_iter().any(|(_, v)| if PART_2 { v == 2 } else { v > 1 })
}

fn main() {
    let mut count = 0;
    for x in RANGE_MIN..=RANGE_MAX {
        if meets_criteria(x) {
            count += 1;
        }
    }
    println!("{} passwords match", count);
}
