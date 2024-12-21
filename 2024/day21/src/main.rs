use std::collections::{HashMap, VecDeque};
use std::error;
use std::io::{self, Read};

mod buttons;

use buttons::{DirButton, NumButton, DIR_BUTTONS, NUMS};
use itertools::Itertools;
use utils::coord::Coord;
use utils::dir::DIRS;

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let codes = buf.lines().collect_vec();

    let p1 = solve(&codes, 2);
    let p2 = solve(&codes, 25);

    println!("Part 1: {p1}\nPart 2: {p2}");

    Ok(())
}

fn solve(codes: &[&str], dpad_robot_count: usize) -> u64 {
    let dir_pairs_iter = DIR_BUTTONS.into_iter().cartesian_product(DIR_BUTTONS);

    // Cost of pressing a button, given a starting position
    let robot_2_costs = dir_pairs_iter
        .clone()
        .map(|(from, to)| {
            (
                (from, to),
                // +1 because this counts pressing As
                Coord::from(from).manhattan_dist(&to.into()) + 1,
            )
        })
        .collect::<HashMap<_, _>>();

    let mut robot_1_costs = robot_2_costs;
    for _ in 1..dpad_robot_count {
        robot_1_costs = DIR_BUTTONS
            .into_iter()
            .flat_map(|from| best_paths_for_dir_robot(from, &robot_1_costs))
            .collect::<HashMap<_, _>>();
    }

    let num_robot_costs = NUMS
        .into_iter()
        .flat_map(|from| num_best_paths(from, &robot_1_costs))
        .collect::<HashMap<_, _>>();

    codes
        .iter()
        .map(|&code| {
            let mut cost = 0;
            let mut num_robot_pos = NumButton::A;
            for char in code.chars() {
                let num = NumButton::try_from(char).unwrap();
                cost += num_robot_costs.get(&(num_robot_pos, num)).unwrap();
                num_robot_pos = num;
            }
            let val: u64 = code[0..code.len() - 1].parse().unwrap();
            val * cost
        })
        .sum()
}

fn best_paths_for_dir_robot(
    from: DirButton,
    // The costs for a previous robot in the chain
    costs: &HashMap<(DirButton, DirButton), u64>,
) -> HashMap<(DirButton, DirButton), u64> {
    #[derive(Debug)]
    struct Search {
        cost: u64,
        curr_robot_pos: DirButton,
        prev_robot_pos: DirButton,
    }
    let mut to_search = VecDeque::from_iter([Search {
        cost: 0,
        curr_robot_pos: from,
        prev_robot_pos: DirButton::A,
    }]);
    let mut distances = HashMap::<(DirButton, DirButton), u64>::new();

    while let Some(search) = to_search.pop_front() {
        let to = search.curr_robot_pos;
        let cost_to_press =
            search.cost + costs.get(&(search.prev_robot_pos, DirButton::A)).unwrap();
        // Possible to already have an entry here if we reached this from a different path
        distances
            .entry((from, to))
            .and_modify(|x| {
                *x = cost_to_press.min(*x);
            })
            .or_insert(cost_to_press);
        for dir in DIRS {
            if let Ok(neighbor) = DirButton::try_from(Coord::from(search.curr_robot_pos).mv(dir)) {
                // I *think* this is okay, but might be problematic if there's a longer path on robot_1 that is actually a shorter path for robot_2
                if distances.contains_key(&(from, neighbor)) {
                    continue;
                }
                let dir_to_press = dir.into();
                let new_cost = costs.get(&(search.prev_robot_pos, dir_to_press)).unwrap();
                to_search.push_back(Search {
                    cost: search.cost + new_cost,
                    prev_robot_pos: dir_to_press,
                    curr_robot_pos: neighbor,
                });
            }
        }
    }
    distances
}

fn num_best_paths(
    from: NumButton,
    costs: &HashMap<(DirButton, DirButton), u64>,
) -> HashMap<(NumButton, NumButton), u64> {
    #[derive(Debug)]
    struct Search {
        cost: u64,
        dir_robot_pos: DirButton,
        num_robot_pos: NumButton,
    }
    let mut to_search = VecDeque::from_iter([Search {
        cost: 0,
        dir_robot_pos: DirButton::A,
        num_robot_pos: from,
    }]);
    let mut distances = HashMap::<(NumButton, NumButton), u64>::new();

    while let Some(search) = to_search.pop_front() {
        let to = search.num_robot_pos;
        let cost_to_press = search.cost + costs.get(&(search.dir_robot_pos, DirButton::A)).unwrap();
        // // Possible to already have an entry here if we reached this from a different path
        distances
            .entry((from, to))
            .and_modify(|x| {
                *x = cost_to_press.min(*x);
            })
            .or_insert(cost_to_press);
        for dir in DIRS {
            if let Ok(neighbor) = NumButton::try_from(Coord::from(search.num_robot_pos).mv(dir)) {
                // I *think* this is okay, but might be problematic if there's a longer path on num_robot that is actually a shorter path for dir_robot
                if distances.contains_key(&(from, neighbor)) {
                    continue;
                }
                let dir_to_press = dir.into();
                let new_cost = costs.get(&(search.dir_robot_pos, dir_to_press)).unwrap();
                to_search.push_back(Search {
                    cost: search.cost + new_cost,
                    dir_robot_pos: dir_to_press,
                    num_robot_pos: neighbor,
                });
            }
        }
    }
    distances
}
