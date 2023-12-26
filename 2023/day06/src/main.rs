use std::io::{self, Read};

type Input = (f64, f64);

fn main() -> Result<(), String> {
    let mut buf = String::new();
    io::stdin()
        .read_to_string(&mut buf)
        .map_err(|_| "Failed to read input")?;

    let (time_line, distance_line) = parse_input(&buf).unwrap();
    let part1_input = parse_part1((time_line, distance_line)).expect("Failed to parse part1");
    let part1: f64 = part1_input.into_iter().map(solve).product();

    let parse_as_single_number = |line: &str| {
        line.split_whitespace()
            .collect::<String>()
            .parse::<f64>()
            .map_err(|_| "Failed to parse")
    };

    let part2 = solve((
        parse_as_single_number(time_line)?,
        parse_as_single_number(distance_line)?,
    ));

    println!("{part1}\n{part2}");
    Ok(())
}

fn solve((time, distance): Input) -> f64 {
    // x * (time - x) > distance
    // -x^2 + time*x - distance > 0
    let (min, max) = quadratic(-1.0, time, -(distance + f64::EPSILON)); // EPSILON, because > 0, not = 0
    max.floor() - min.floor()
}

fn quadratic(a: f64, b: f64, c: f64) -> (f64, f64) {
    let sqrt_term = (b * b - 4.0 * a * c).sqrt();
    ((-b + sqrt_term) / 2.0 / a, (-b - sqrt_term) / 2.0 / a)
}

fn parse_input(buf: &str) -> Option<(&str, &str)> {
    let mut lines = buf.lines();
    let time_line = lines.next()?.strip_prefix("Time:")?.trim();
    let distance_line = lines.next()?.strip_prefix("Distance:")?.trim();
    Some((time_line, distance_line))
}

fn parse_part1((time_line, distance_line): (&str, &str)) -> Option<Vec<Input>> {
    time_line
        .split_ascii_whitespace()
        .map(|x| x.parse::<f64>().ok())
        .zip(
            distance_line
                .split_ascii_whitespace()
                .map(|x| x.parse::<f64>().ok()),
        )
        .map(|(x, y)| Some((x?, y?)))
        .collect()
}
