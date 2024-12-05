use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    io::{self, Read},
};

struct Rule {
    before: u32,
    after: u32,
}
type Update = Vec<u32>;

fn parse_input(input: &str) -> Option<(Vec<Rule>, Vec<Update>)> {
    let (rules, updates) = input.trim().splitn(2, "\n\n").collect_tuple()?;

    let rules = rules
        .split("\n")
        .map(|line| {
            let (before, after) = line
                .splitn(2, "|")
                .map(|x| x.parse::<u32>().expect("Failed to parse ordering"))
                .collect_tuple()
                .unwrap();
            Rule { before, after }
        })
        .collect_vec();

    let updates = updates
        .split("\n")
        .map(|line| {
            line.split(",")
                .map(|x| x.parse::<u32>().expect("Failed to parse update"))
                .collect_vec()
        })
        .collect_vec();
    Some((rules, updates))
}

fn is_ordered(must_precede: &HashMap<u32, HashSet<u32>>, update: &Update) -> bool {
    let mut seen = HashSet::<u32>::new();
    for &record in update.iter() {
        if must_precede
            .get(&record)
            .map(|precede_set| !precede_set.is_disjoint(&seen))
            .unwrap_or(false)
        {
            return false;
        }
        seen.insert(record);
    }
    true
}
fn fix_update(must_precede: &HashMap<u32, HashSet<u32>>, update: &Update) -> Update {
    let mut new_update = Update::with_capacity(update.len());
    let mut to_add = HashSet::new();
    for update in update.iter() {
        to_add.insert(*update);
    }
    while !to_add.is_empty() {
        let new_to_add = *to_add
            .iter()
            .find(|&&u| {
                must_precede
                    .get(&u)
                    .map(|precede_set| precede_set.is_disjoint(&to_add))
                    .unwrap_or(true)
            })
            .expect("Couldn't find one to add");
        to_add.remove(&new_to_add);
        // This is actually backwards - we find each each element that should be first, but we're pushing it at the end
        // but we only actually need the midpoint so reversed is just as good
        new_update.push(new_to_add);
    }
    new_update
}

fn get_midpoint(update: &Update) -> u32 {
    let midpoint = (update.len() - 1) / 2;
    *update.get(midpoint).expect("Failed to get midpoint")
}
fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let (rules, updates) = parse_input(&buffer).unwrap();

    let mut must_precede: HashMap<u32, HashSet<u32>> = HashMap::new();
    for rule in rules {
        must_precede
            .entry(rule.before)
            .or_default()
            .insert(rule.after);
    }

    let (correct, incorrect): (Vec<_>, Vec<_>) = updates
        .into_iter()
        .partition(|u| is_ordered(&must_precede, u));

    let part1: u32 = correct.iter().map(get_midpoint).sum();
    let part2: u32 = incorrect
        .into_iter()
        .map(|u| fix_update(&must_precede, &u))
        .map(|u| get_midpoint(&u))
        .sum();

    println!("Part 1: {part1}\nPart 2: {part2}");

    Ok(())
}
