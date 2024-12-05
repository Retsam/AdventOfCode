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

fn parse_input(input: &str) -> (Vec<Rule>, Vec<Update>) {
    let (rules, updates) = input.trim().splitn(2, "\n\n").collect_tuple().unwrap();

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
    (rules, updates)
}

fn is_ordered(must_precede: &HashMap<u32, HashSet<u32>>, update: &Update) -> bool {
    let mut seen = HashSet::<u32>::new();
    for &record in update.iter() {
        if let Some(true) = must_precede
            .get(&record)
            .map(|precede_set| !precede_set.is_disjoint(&seen))
        {
            return false;
        }
        seen.insert(record);
    }
    true
}
fn sort_update(must_precede: &HashMap<u32, HashSet<u32>>, update: &mut Update) {
    // Empty the array into a HashSet and rebuild it
    let mut items_to_insert = HashSet::from_iter(update.drain(..));
    while !items_to_insert.is_empty() {
        let add_next = *items_to_insert
            .iter()
            .find(|item| {
                // Find an item that doesn't have to come before anything else still in the set
                //     Either it has no rules: `unwrap_or(true)`
                //       or else the rules set doesn't overlap with anything still to insert
                must_precede
                    .get(item)
                    .map(|precede_set| precede_set.is_disjoint(&items_to_insert))
                    .unwrap_or(true)
            })
            .expect("Couldn't find one to add");
        items_to_insert.remove(&add_next);
        // This is actually backwards - we find each each element that should be first, but we're pushing it at the end
        // but we only actually need the midpoint so reversed is just as good
        update.push(add_next);
    }
}

fn get_midpoint(update: Update) -> u32 {
    let midpoint = (update.len() - 1) / 2;
    *update.get(midpoint).expect("Failed to get midpoint")
}
fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let (rules, updates) = parse_input(&buffer);

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

    let part1: u32 = correct.into_iter().map(get_midpoint).sum();
    let part2: u32 = incorrect
        .into_iter()
        .update(|u| sort_update(&must_precede, u))
        .map(get_midpoint)
        .sum();

    println!("Part 1: {part1}\nPart 2: {part2}");

    Ok(())
}
