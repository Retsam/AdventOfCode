use std::error;
use std::io::{self, Read};

#[derive(Debug, Clone)]
struct Chunk {
    // idx: u32,
    size: u32,
    data: Option<u32>,
}

#[derive(Clone)]
struct Chunks {
    vec: Vec<Chunk>,
}
impl Chunks {
    fn trim(&mut self) {
        while let Some(Chunk { data: None, .. }) = self.vec.last() {
            self.vec.pop();
        }
    }
    fn find_gap(&self) -> Option<usize> {
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

    fn score(&self) -> u64 {
        let mut idx = 0;
        let score: u64 = self
            .vec
            .iter()
            .map(|c| {
                let new_idx = idx + c.size;
                let r = c
                    .data
                    .map(|data| (idx..new_idx).map(|i| i as u64 * data as u64).sum())
                    .unwrap_or(0);
                idx = new_idx;
                r
            })
            .sum();
        score
    }
    #[allow(unused)]
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

fn solve_part1(mut chunks: Chunks) -> u64 {
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
        // If there's leftover adjust the chunk size and put it back
        if last.size > gap_size {
            last.size -= gap_size;
            chunks.vec.push(last);
        } else {
            chunks.trim();
        }
    }

    chunks.score()
}
fn solve_part2(mut chunks: Chunks, max_id: u32) -> u64 {
    chunks.trim();

    let mut file_to_move = max_id;
    while file_to_move > 0 {
        let (i, _moving) = chunks
            .vec
            .iter()
            .enumerate()
            .find(|(_, c)| c.data == Some(file_to_move))
            .unwrap_or_else(|| panic!("failed to get {file_to_move}"));
        let moving = _moving.clone();
        let moving_data = moving.data.unwrap();

        if let Some(gap_idx) = chunks
            .vec
            .iter()
            .enumerate()
            .position(|(idx, gap)| idx < i && gap.data.is_none() && gap.size >= moving.size)
        {
            let gap = &mut chunks.vec[gap_idx];
            let moved = if gap.size == moving.size {
                chunks.fill_gap(gap_idx, moving_data);
                &mut chunks.vec[i]
            } else {
                chunks.partial_fill_gap(gap_idx, moving.size, moving_data);
                &mut chunks.vec[i + 1]
            };
            moved.data = None;
        }

        file_to_move -= 1;
    }
    chunks.score()
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut input = String::new();
    io::stdin()
        .read_to_string(&mut input)
        .map_err(|_| "Failed to read input")?;

    let mut iter = input.trim().chars().map(|c| c.to_digit(10).unwrap());

    let mut id = 0;
    let mut chunks: Vec<Chunk> = vec![];
    while let Some(chunk) = iter.next() {
        if chunk > 0 {
            chunks.push(Chunk {
                size: chunk,
                data: Some(id),
            });
        }
        id += 1;
        if let Some(skip) = iter.next() {
            chunks.push(Chunk {
                size: skip,
                data: None,
            });
        }
    }
    let chunks = Chunks { vec: chunks };
    let part1 = solve_part1(chunks.clone());
    let part2 = solve_part2(chunks, id - 1);

    println!("Part 1: {part1}\nPart 2: {part2}");
    Ok(())
}
