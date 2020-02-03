use std::io::{self, Read};

fn pattern_iter() -> impl Iterator<Item = i32> {
    [0, 1, 0, -1].into_iter().cloned().cycle()
}

struct Repeater(i32);

impl Iterator for Repeater {
    type Item = i32;
    fn next(&mut self) -> Option<Self::Item> {
        Some(self.0)
    }
}

fn fft_signal(offset: usize) -> impl Iterator<Item = i32> {
    pattern_iter()
        .flat_map(move |x| Repeater(x).take(offset + 1))
        .skip(1)
}

struct FFT(Vec<i32>);

impl FFT {
    fn phase(input: &Vec<i32>) -> Vec<i32> {
        let mut output = vec!();
        for i in 0..input.len() {
            let sum: i32 = fft_signal(i).zip(input.iter())
                .map(|(a, b)| a * b)
                .sum();
            let last_digit = (sum % 10).abs();
            output.push(last_digit);
        }
        output
    }
}

impl Iterator for FFT {
    type Item = Vec<i32>;
    fn next(&mut self) -> Option<Self::Item> {
        self.0 = FFT::phase(&self.0);
        Some(self.0.clone())
    }
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let line = buffer.lines().next().unwrap();

    let mut fft = FFT(
        line.chars()
            .map(|x| x.to_digit(10).unwrap() as i32)
            .collect()
    );
    let result: String = fft.nth(99)
        .unwrap()
        // vec to string
        .iter().map(|x| x.to_string())
        .take(8).collect();
    println!("{}", result);

    Ok(())
}
