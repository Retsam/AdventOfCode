use std::collections::HashSet;
use std::collections::VecDeque;

#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone)]
struct Coord(u32, u32);
impl Coord {
    fn adjacent(&self) -> Vec<Coord> {
        let mut adj = vec![
            Coord (self.0 + 1, self.1),
            Coord (self.0, self.1 + 1),
        ];
        if self.0 > 0 {
            adj.push(Coord (self.0 - 1, self.1));
        }
        if self.1 > 0 {
            adj.push(Coord (self.0, self.1 - 1));
        }
        adj
    }
}

// const FAV_NUM: u32 = 10;
// const GOAL: Coord = Coord(7, 4);
const FAV_NUM: u32 = 1362;
// const GOAL: Coord = Coord(31, 39);

//Wall building
fn sum_bits(mut number: u32) -> u32 {
    let mut count = 0;
    while number > 0 {
        if number & 1 == 1 {
            count += 1;
        }
        number >>= 1;
    }
    count
}

fn is_wall(coord: &Coord) -> bool {
    let &Coord(x, y) = coord;
    let number = x*x + 3*x + 2*x*y + y + y*y + FAV_NUM;
    sum_bits(number) % 2 == 1
}

fn main() {
    let init_pos = Coord (1, 1);

    let mut search_queue = VecDeque::new();
    search_queue.push_back((init_pos, 0));

    let mut visited = HashSet::new();

    while let Some((pos, step_count)) = search_queue.pop_front() {
        if visited.contains(&pos) {
            continue;
        }
        visited.insert(pos);

        if step_count == 50 {
            continue
        }

        for new_pos in pos.adjacent().iter().filter(|pos| !is_wall(pos)) {
            // if pos == GOAL {
            //     println!("Got there in {}", step_count);
            //     return;
            // }
            search_queue.push_back((*new_pos, step_count+1));
        }


    }
    println!("Done, visited {} places", visited.len());
    // println!("  0123456789");
    // for y in 0..6 {
    //     print!("{} ", y);
    //     for x in 0..10 {
    //         if is_wall(x,y) {
    //             print!("X");
    //         } else {
    //             print!(".");
    //         }
    //     }
    //     println!("");
    // }
}
