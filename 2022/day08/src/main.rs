use std::{
    collections::HashSet,
    io::{self, Read},
};

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    Ok(buffer)
}

fn main() -> io::Result<()> {
    let input = read_input()?;
    let trees = input
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| c.to_digit(10).unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let max = trees.len(); // input is a square

    let mut visible_from_edge = HashSet::<(usize, usize)>::new();
    let mut highest_score = 0;
    let mut scores = vec![vec![1; max - 2]; max - 2];

    let mut update = |vista: &mut Vec<u32>, x: usize, y: usize| {
        let tree = trees[y][x];
        let mut seen = vista.iter().rev().take_while(|&&x| x < tree).count();
        if seen == vista.len() {
            visible_from_edge.insert((x, y));
        } else {
            seen += 1;
        }

        let score = &mut scores[y - 1][x - 1];
        *score *= seen;
        highest_score = highest_score.max(*score);
        vista.push(tree);
    };

    let range = || 1..(max - 1);
    for y in range() {
        let mut vista = vec![trees[y][0]];
        for x in range() {
            update(&mut vista, x, y);
        }
        let mut vista = vec![trees[y][max - 1]];
        for x in range().rev() {
            update(&mut vista, x, y);
        }
    }
    for x in range() {
        let mut vista = vec![trees[0][x]];
        for y in range() {
            update(&mut vista, x, y);
        }
        let mut vista = vec![trees[max - 1][x]];
        for y in range().rev() {
            update(&mut vista, x, y);
        }
    }

    println!(
        "Part 1: {}\nPart 2: {}",
        visible_from_edge.len() + 4 * (max - 1),
        highest_score
    );

    Ok(())
}
