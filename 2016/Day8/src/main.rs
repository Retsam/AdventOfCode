use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

const FILE_NAME: &'static str = "input.txt";

#[derive(Debug)]
enum Command {
    Rect(usize, usize),
    Rotate {what: Rotate, which:usize, by: u8}
}

#[derive(Debug)]
enum Rotate {
    Row,
    Col
}

fn parse_line(line: &str) -> Command {
    let mut words = line.split_whitespace();
    match words.next().unwrap() {
        "rect" => {
            let mut dim = words.next().unwrap() //e.g "6x4"
                .split("x");
            let (x,y) = (
                dim.next().unwrap().parse::<usize>().unwrap(),
                dim.next().unwrap().parse::<usize>().unwrap()
            );
            Command::Rect(x,y)
        },
        "rotate" => {
            let what = match words.next().unwrap() {
                "row" => Rotate::Row,
                "column" => Rotate::Col,
                unexpected => panic!("Unexpected word after 'rotate': {}", unexpected)
            };
            let which = words.next().unwrap() //y=10 or x=10
                [2..]
                .parse::<usize>().unwrap();
            words.next(); //by
            let by = words.next().unwrap()
                .parse::<u8>().unwrap();
            Command::Rotate {what:what, which:which, by:by}
        },
        unexpected => panic!("Unexpeted word {}", unexpected)
    }
}

const WIDTH: usize = 50;
const HEIGHT: usize = 6;

fn print_screen(screen: &Vec<bool>) {
    for y in 0..HEIGHT {
        for x in 0..WIDTH {
            print!("{}", if screen[y*WIDTH + x] {"X"} else {"."});
        }
        println!("");
    }
}

fn rotate(screen: &mut Vec<bool>, what: &Rotate, which: usize, by: u8) {
    for _ in 0..by {
        rotate_once(screen, what, which);
    }
}
fn rotate_once(screen: &mut Vec<bool>, what: &Rotate, which: usize) {
    let get_nth_row = |n| which * WIDTH + n;
    let get_nth_col = |n| n * WIDTH + which;
    let get_nth = |n| match *what {Rotate::Row => get_nth_row(n), Rotate::Col => get_nth_col(n)};

    let last_index = match *what {Rotate::Row => WIDTH-1, Rotate::Col => HEIGHT-1};
    let last = screen[get_nth(last_index)];
    for i in 0..last_index {
        // println!("{}", get_nth(last_index-i));
        screen[get_nth(last_index-i)] = screen[get_nth(last_index-(i+1))];
    }
    screen[get_nth(0)] = last;
}

fn main() {
    let f = File::open(FILE_NAME).unwrap();
    let i = BufReader::new(f);
    let lines = i.lines().map(|l| l.unwrap());

    let mut screen = vec![false; WIDTH * HEIGHT];

    let commands = lines.map(|line| parse_line(&line));
    // let commands = vec![
    //     Command::Rect(2,3),
    //     Command::Rotate{what: Rotate::Row, by: 2, which: 2},
    //     Command::Rotate{what: Rotate::Col, by: 7, which: 1}
    // ];

    for command in commands {
        println!("{:?}", command);
        match command {
            Command::Rect(w, h) => {
                for x in 0..w {
                    for y in 0..h {
                        screen[y*WIDTH + x] = true
                    }
                }
            },
            Command::Rotate {what, which, by} => {
                rotate(&mut screen, &what, which, by);
            }
        }
        print_screen(&screen);
    }

    let count = screen.iter().filter(|b| **b).count();
    println!("{} cells are active", count);
}
