use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
// use std::iter::FromIterator;
use std::collections::HashMap;
use std::mem::replace;
//
const FILE_NAME: &'static str = "input.txt";

type BotId = u8;
type ChipValue = u8;
type BotStates = HashMap<BotId, Box<BotState>>;
type OutputStates = HashMap<BotId, ChipValue>;

#[derive(Debug)]
enum Destination {
    Bot(BotId),
    Output(BotId)
}

#[derive(Debug)]
struct BotCommand {
    high_to: Destination,
    low_to: Destination
}

#[derive(Debug)]
enum Instruction {
    BotCommand(BotId, BotCommand),
    ValueTo { value: ChipValue, to: BotId }
}

#[derive(Debug)]
struct BotState {
    command: Option<BotCommand>,
    high_value: Option<ChipValue>,
    low_value: Option<ChipValue>
}

fn get_bot_state(bot_states: &mut BotStates, bot_id: BotId) -> &mut Box<BotState> {
    bot_states.entry(bot_id).or_insert(Box::new(BotState {
        command: None,
        high_value: None,
        low_value: None
    }))
}

fn give_value(mut bot_states: &mut BotStates, mut output_states: &mut OutputStates, destination: &Destination, value: ChipValue) {
    match *destination {
        Destination::Bot(bot_id) => {
            {
                let bot_state = get_bot_state(&mut bot_states, bot_id);
                // println!("\tState {} was {:?}", bot_id, bot_state);
                if let Some(old_value) = bot_state.high_value {
                    if value > old_value {
                        bot_state.high_value = Some(value);
                        bot_state.low_value = Some(old_value);
                    } else {
                        bot_state.low_value = Some(value);
                    }
                } else {
                    bot_state.high_value = Some(value);
                }
                // println!("\tState {} is now {:?}", bot_id, bot_state);
            }
            maybe_execute_command(&mut bot_states, &mut output_states, bot_id)

        }
        Destination::Output(output_id) => {
            output_states.insert(output_id, value);
        }
    }
}

fn maybe_execute_command(mut bot_states: &mut BotStates, mut output_states: &mut OutputStates, bot_id: BotId) {
    {
        let bot_state = get_bot_state(&mut bot_states, bot_id);

        if bot_state.high_value.is_none()
            || bot_state.low_value.is_none()
            || bot_state.command.is_none() {
            return;
        }
    }
    let boxed_bot_state = replace(get_bot_state(&mut bot_states, bot_id), Box::new(BotState {command: None, high_value: None, low_value:None}));

    let bot_state = *boxed_bot_state;
    let BotState {command, high_value, low_value} = bot_state;
    match command.unwrap() {
        BotCommand {ref high_to, ref low_to} => {
            let hv = high_value.unwrap();
            let lv = low_value.unwrap();
            println!("{} is giving {} to {:?} and {} to {:?}", bot_id, hv, high_to, lv, low_to);
            give_value(&mut bot_states, &mut output_states, high_to, hv);
            give_value(&mut bot_states, &mut output_states, low_to, lv);
        }
    }
}

fn parse_destination(words: &mut Iterator<Item=&str>) -> Destination {
    let destination_type = words.skip(3)            // "gives low to" or "and high to"
                                .next().unwrap();   // "bot" or "output"
    let destination_id = words.next().unwrap().parse::<BotId>().unwrap(); //id
    match destination_type {
        "bot" => Destination::Bot(destination_id),
        "output" => Destination::Output(destination_id),
        unexpected => panic!("Unexpected destination type {}", unexpected)
    }
}

fn parse_line(line: String) -> Instruction {
    let mut words = line.split_whitespace();
    if words.next().unwrap() == "bot" {
        let bot_id = words.next().unwrap().parse::<BotId>().unwrap();
        Instruction::BotCommand(bot_id, BotCommand {
            low_to: parse_destination(&mut words),
            high_to: parse_destination(&mut words),
        })
    } else {
        Instruction::ValueTo {
            value: words.next().unwrap().parse::<ChipValue>().unwrap(),
            to: words.skip(3).next().unwrap().parse::<BotId>().unwrap(),
        }
    }
}

fn main() {
    let f = File::open(FILE_NAME).unwrap();
    let i = BufReader::new(f);

    let lines = i.lines().map(|l| l.unwrap());
    let instructions = lines.map(|line| parse_line(line));

    let mut bot_states: BotStates = HashMap::new();
    let mut output_states: OutputStates = HashMap::new();

    // let instructions = vec![
    //     Instruction::BotCommand(1, BotCommand { high_to: 2, low_to: 3}),
    //     Instruction::ValueTo {value: 10, to: 1},
    //     Instruction::ValueTo {value: 9, to: 1}
    // ];

    for instruction in instructions {
        match instruction {
            Instruction::BotCommand(from, command) => {
                {
                    // println!("Commanding {}: \n\t{:?}", from, command);
                    let mut bot_state = get_bot_state(&mut bot_states, from);
                    // println!("\tState was {:?}", bot_state);
                    bot_state.command = Some(command);
                }
                maybe_execute_command(&mut bot_states, &mut output_states, from);
            },
            Instruction::ValueTo {value, to} => {
                println!("Giving {} to {}", value, to);
                give_value(&mut bot_states, &mut output_states, &Destination::Bot(to), value)
            }
        }
    }
    // println!("{:#?}", bot_states);
}
