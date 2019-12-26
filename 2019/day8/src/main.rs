use std::io::{self, Read};

type Val = u32;

const WIDTH: usize = 25;
const HEIGHT: usize = 6;

fn chunk_by(input: &Vec<Val>) -> Vec<Vec<Val>> {
    input.iter().cloned().enumerate().fold(
        (Vec::new(), Vec::new()),
        |(mut vecs, mut vec), (i, v)| {
            vec.push(v);
            if (i + 1) % (HEIGHT * WIDTH) == 0 {
                vecs.push(vec);
                (vecs, Vec::new())
            } else {
                (vecs, vec)
            }
        }
    ).0
}

fn count_digits(layer: &Vec<Val>, digit: Val) -> usize {
    layer.iter().cloned().filter(|x| { *x == digit }).count()
}

// Part 2
fn build_image(input: &Vec<Val>) -> [[Val; WIDTH]; HEIGHT] {
    let mut data = [[2; WIDTH]; HEIGHT];
    for x in 0..WIDTH {
        for y in 0..HEIGHT {
            for z in ((y * WIDTH + x)..input.len()).step_by(WIDTH*HEIGHT) {
                if input[z] != 2 {
                    data[y][x] = input[z];
                    break;
                }
            }
        }
    }
    data
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let data: Vec<Val> = buffer.lines().next().unwrap()
        .chars().map(|c| c.to_digit(10).unwrap()).collect();

    let layers = chunk_by(&data);
    let least_zeroes = layers.iter().min_by_key(|layer| {
        count_digits(layer, 0)
    }).expect("No layers");
    println!("Checksum is {:?}", count_digits(least_zeroes, 1) * count_digits(least_zeroes, 2));

    let result = build_image(&data);
    let output = result.iter().map(|row| {
        row.iter().map(|x| {
            if *x == 1 { "X" } else { " " }
        }).collect::<Vec<_>>().concat()
    }).collect::<Vec<_>>().join("\n");
    println!("Image is:\n{}", output);

    Ok(())
}
