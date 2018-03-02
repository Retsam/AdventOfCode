extern crate crypto;

use crypto::md5::Md5;
use crypto::digest::Digest;

const ROOM_ID: &'static str = "wtnhxymk";

fn md5(in_str: & str) -> String {
    let mut sh = Md5::new();
    sh.input_str(in_str);
    sh.result_str()
}

fn main() {
    let mut index = -1;
    // let mut code = vec![];
    let mut code = [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '];
    let mut code_idx = 0;
    loop {
        index += 1;

        let in_str = ROOM_ID.to_string() + &index.to_string();
        let res = md5(&in_str);

        if res.chars().take(5).collect::<String>() == "00000" {

            /* //VERSION ONE
            println!("Got one md5({}) = {}", in_str, res);
            let next = res.chars().skip(5).next().unwrap();
            println!("Got char {}", next);
            code.push(next);
            code_idx += 1;
            if code_idx == 8 {
                let code_str: String = code.into_iter().collect();
                println!("The code is {}", code_str);
                break;
            }
            */

            //VERSION TWO
            println!("Got one md5({}) = {}", in_str, res);
            let position = res.chars().skip(5).next().unwrap()
                .to_string().parse::<usize>().unwrap_or(8);
            if position >= 8 || code[position] != ' ' {
                continue;
            }
            let next_char = res.chars().skip(6).next().unwrap();
            println!("Got char {} in position {}", next_char, position);
            code[position] = next_char;
            code_idx += 1;
            if code_idx == 8 {
                let code_str: String = code.into_iter().map(|x| *x).collect();
                println!("The code is {}", code_str);
                break;
            }
        }
    }
}
