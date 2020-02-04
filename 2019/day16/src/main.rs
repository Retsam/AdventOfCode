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

#[derive(Debug)]
struct SuperFFT(Vec<i32>, i32);

impl SuperFFT {
    fn new(input: &Vec<i32>, range: std::ops::Range<usize>) -> SuperFFT {
        let mut sum = 0;
        let vec: Vec<_> = range.map(|i| {
            let v = input[i % input.len()];
            sum = (sum + v) % 10;
            v
        }).collect();
        SuperFFT(vec, sum)
    }
}

impl Iterator for SuperFFT {
    type Item = Vec<i32>;
    fn next(&mut self) -> Option<Self::Item> {
        let input = &self.0;
        let mut sum = self.1;
        let mut new_sum = 0;
        let new: Vec<_> = input.iter()
            .map(|v| {
                let n = sum;
                new_sum = (new_sum + n) % 10;
                sum = (sum - v + 10) % 10;
                n
            }).collect();
        self.0 = new;
        self.1 = new_sum;

        Some(self.0.clone())
    }
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let line = buffer.lines().next().unwrap();

    let input: Vec<_> = line.chars()
        .map(|x| x.to_digit(10).unwrap() as i32)
        .collect();
    let offset: usize = line.chars().take(7).collect::<String>().parse().unwrap();

    let mut fft = FFT(input.clone());
    let result: String = fft.nth(99)
        .unwrap()
        // vec to string
        .iter().take(8).map(|x| x.to_string()).collect();
    println!("Part 1 - {}", result);

    let mut super_fft = SuperFFT::new(&input, offset..input.len()*10000);
    for _ in 0..100 {
        super_fft.next();
    }

    let SuperFFT(res, _) = super_fft;
    let code: String = res.iter().cloned()
        .take(8)
        .map(|x| x.to_string())
        .collect();
    println!("Part 2 - {}", code);

    Ok(())
}
