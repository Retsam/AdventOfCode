use itertools::Itertools;
use std::io::{self, Read};

fn is_safe(report: &[i32]) -> bool {
    let mut diffs = report.windows(2).map(|x| x[1] - x[0]).peekable();
    let sign = diffs
        .peek()
        .expect("Expected report to have at least two items")
        .signum();

    sign != 0 && diffs.all(|d| d.signum() == sign && d.abs() <= 3)
}
fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let reports = buffer
        .lines()
        .map(|line| {
            line.split(" ")
                .map(|x| x.parse::<i32>().unwrap())
                .collect_vec()
        })
        .collect_vec();

    let without = |vec: &Vec<_>, i| {
        let mut v2 = vec.clone();
        v2.remove(i);
        v2
    };

    let p1 = reports.iter().filter(|x| is_safe(x)).count();
    let p2 = reports
        .iter()
        .filter(|report| (0..report.len()).any(|i| is_safe(&without(report, i))))
        .count();

    println!("Part 1:{p1:?}\nPart 2: {p2:?}");
    Ok(())
}
