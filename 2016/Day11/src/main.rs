use std::collections::{HashSet, VecDeque};
use std::hash::{Hash, SipHasher, Hasher};

type Floor = u8;
type ElementType = &'static str;

#[derive(PartialEq, Copy, Clone, Debug)]
enum Item {
    Chip(ElementType),
    Generator(ElementType)
}

#[derive(Clone, Debug)]
struct Element {
    name: ElementType,
    chip_floor: Floor,
    generator_floor: Floor
}

#[derive(Debug)]
struct GameState {
    elements: Vec<Element>,
    player_floor: Floor
}

#[derive(Debug)]
struct GameStateAndCount {
    state: GameState,
    count: u8
}

fn get_items_on_current_floor(state: &GameState) -> Vec<Item> {
    let mut items_on_same_floor = vec![];
    for element in &state.elements {
        if element.chip_floor == state.player_floor {
            items_on_same_floor.push(Item::Chip(element.name))
        }
        if element.generator_floor == state.player_floor {
            items_on_same_floor.push(Item::Generator(element.name))
        }
    }
    items_on_same_floor
}

type PossibleMove = (Vec<Item>, Floor);

fn get_next_states(state_and_count: &GameStateAndCount) -> Vec<GameStateAndCount> {
    let mut new_states = vec![];
    let possible_moves = get_possible_moves(&state_and_count.state);
    for possible_move in possible_moves {
        new_states.push(get_new_state(&state_and_count, possible_move));
    }
    new_states
}

fn get_possible_moves(state: &GameState) -> Vec<PossibleMove> {
    let items_on_current_floor = get_items_on_current_floor(&state);
    let current_floor = state.player_floor;

    let mut possible_moves = vec![];
    let mut destination_floors = vec![];
    if current_floor > 1 { destination_floors.push(current_floor - 1) }
    if current_floor < 4 { destination_floors.push(current_floor + 1) };

    for item1 in &items_on_current_floor {
        for item2 in &items_on_current_floor {
            for floor in &destination_floors {
                if item1 == item2 {
                    possible_moves.push((vec![item1.clone()], *floor))
                } else {
                    possible_moves.push((vec![item1.clone(), item2.clone()], *floor))
                }
            }
        }
    }

    possible_moves
}

fn get_new_state(state_and_count: &GameStateAndCount, _move: PossibleMove) -> GameStateAndCount {
    let mut elements = state_and_count.state.elements.clone();
    let (items, floor) = _move;
    for item in items {
        match item {
            Item::Chip(element_name) => {
                for element in &mut elements {
                    if element.name == element_name {
                        element.chip_floor = floor;
                    }
                }
            },
            Item::Generator(element_name) => {
                for element in &mut elements {
                    if element.name == element_name {
                        element.generator_floor = floor;
                    }
                }
            }
        }
    }
    GameStateAndCount {
        state: GameState {
            elements: elements,
            player_floor: floor
        },
        count: state_and_count.count + 1
    }
}

fn state_is_valid(state: &GameState) -> bool {
    let mut generator_floors = HashSet::new();
    for element in &state.elements {
        generator_floors.insert(element.generator_floor);
    }
    state.elements.iter().all(|element| {
        //Either the chip is with the generator
        element.chip_floor == element.generator_floor
        //Or else there aren't any other generators on the floor
            || !generator_floors.contains(&element.chip_floor)
    })
}

fn state_is_win(state: &GameState) -> bool {
    state.elements.iter().all(|element| {
        element.chip_floor == 4 && element.generator_floor == 4
    })
}

#[derive(Hash)]
struct GameStateHashable {
    elements: Vec<(Floor, Floor)>,
    player_floor: Floor
}

fn hash(game_state: &GameState) -> u64 {
    let mut s = SipHasher::new();

    let mut hashable_elements: Vec<_> = game_state.elements.iter().map(|element| (element.chip_floor, element.generator_floor)).collect();
    hashable_elements.sort();
    let game_state_hashable = GameStateHashable {
        elements: hashable_elements,
        player_floor: game_state.player_floor
    };
    game_state_hashable.hash(&mut s);
    s.finish()
}

fn main() {

    // let initial_state = GameState {
    //     elements: vec![
    //         Element { name: "Hydrogen", chip_floor: 1, generator_floor: 2},
    //         Element { name: "Lithium", chip_floor: 1, generator_floor: 3}
    //     ],
    //     player_floor: 1
    // };

    let initial_state = GameState {
        elements: vec![
            Element { name: "Polonium", chip_floor: 2, generator_floor: 1},
            Element { name: "Thulium", chip_floor: 1, generator_floor: 1},
            Element { name: "Promethium", chip_floor: 2, generator_floor: 1},
            Element { name: "Ruthenium", chip_floor: 1, generator_floor: 1},
            Element { name: "Cobalt", chip_floor: 1, generator_floor: 1},
            Element { name: "Elerium", chip_floor: 1, generator_floor: 1},
            Element { name: "Dilithium", chip_floor: 1, generator_floor: 1}
        ],
        player_floor: 1
    };

    let mut seen_states = HashSet::new();

    println!("{:#?}", initial_state);

    let initial_state_and_count = GameStateAndCount {state: initial_state, count: 0};
    // println!("Possible moves: {:#?}", get_possible_moves(get_items_on_current_floor(&initial_state_and_count.state), initial_state_and_count.state.player_floor));

    let mut states_to_test = VecDeque::new();

    states_to_test.push_back(initial_state_and_count);

    'outer: while let Some(state_and_count) = states_to_test.pop_front() {
        let new_hash = hash(&state_and_count.state);
        if seen_states.contains(&new_hash) {
            println!("Found a duplicate {}", new_hash);
            continue;
        } else {
            println!("Found a new one: {}", new_hash);
            seen_states.insert(new_hash);
        }
        if !state_is_valid(&state_and_count.state) {
            println!("Invalid");
            continue
        }
        let next_states = get_next_states(&state_and_count);
        println!("Got {} new states", next_states.len());
        // println!("{:#?}", next_states);
        for new_state in next_states {
            if state_is_win(&new_state.state) {
                println!("Found winner!");
                println!("{:#?}", new_state);
                break 'outer;
            }
            states_to_test.push_back(new_state);
        }
    };

}
