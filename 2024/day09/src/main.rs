use std::error;
use std::io::{self, Read};

use itertools::Itertools;

#[derive(Debug)]
struct Chunk {
    // idx: u32,
    size: u32,
    data: Option<u32>,
}

struct Chunks {
    vec: Vec<Chunk>,
}
impl Chunks {
    fn new() -> Self {
        Self { vec: vec![] }
    }
    fn trim(&mut self) {
        while let Some(Chunk { data: None, .. }) = self.vec.last() {
            self.vec.pop();
        }
    }
    fn find_gap(&self) -> Option<usize> {
        // self.vec.iter().enumerate().find(|(_, c)| c.data.is_none())
        self.vec.iter().position(|c| c.data.is_none())
    }
    fn partial_fill_gap(&mut self, gap_idx: usize, size: u32, data: u32) {
        let gap = &mut self.vec[gap_idx];
        gap.size -= size;
        self.vec.insert(
            gap_idx,
            Chunk {
                size,
                data: Some(data),
            },
        );
    }
    fn fill_gap(&mut self, gap_idx: usize, data: u32) {
        let gap = &mut self.vec[gap_idx];
        gap.data = Some(data);
    }
    fn debug(&self) {
        for chunk in &self.vec {
            for _ in 0..chunk.size {
                print!(
                    "{}",
                    chunk.data.map(|x| x.to_string()).unwrap_or(".".to_string())
                );
            }
        }
        println!();
    }
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut input = String::new();
    io::stdin()
        .read_to_string(&mut input)
        .map_err(|_| "Failed to read input")?;

    let mut iter = input.trim().chars().map(|c| c.to_digit(10).unwrap());

    let mut idx = 0;
    let mut id = 0;
    let mut chunks: Vec<Chunk> = vec![];
    while let Some(chunk) = iter.next() {
        if chunk > 0 {
            chunks.push(Chunk {
                // idx,
                size: chunk,
                data: Some(id),
            });
            idx += chunk;
        }
        id += 1;
        if let Some(skip) = iter.next() {
            chunks.push(Chunk {
                // idx,
                size: skip,
                data: None,
            });
            idx += skip;
        }
    }
    let mut chunks = Chunks { vec: chunks };
    // chunks.debug();

    chunks.trim();

    while let Some(gap_idx) = chunks.find_gap() {
        let mut last = chunks.vec.pop().expect("Oops no more chunks");
        let data = last.data.expect("Unexpected gap");
        let gap = &chunks.vec[gap_idx];
        let gap_size = gap.size;
        if gap_size > last.size {
            chunks.partial_fill_gap(gap_idx, last.size, data);
        } else {
            chunks.fill_gap(gap_idx, data);
        }
        if last.size > gap_size {
            last.size -= gap_size;
            chunks.vec.push(last);
        } else {
            chunks.trim();
        }
        // chunks.debug();
    }

    let mut idx = 0;
    let part1: u64 = chunks
        .vec
        .iter()
        .map(|c| {
            let new_idx = idx + c.size;
            let data = c.data.expect("Unexpected gap");
            let r: u64 = (idx..new_idx).map(|i| i as u64 * data as u64).sum();
            idx = new_idx;
            r
        })
        .sum();
    println!("Part 1: {part1}");
    Ok(())
}
