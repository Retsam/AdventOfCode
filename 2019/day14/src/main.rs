use std::io::{self, Read};
use core::str::FromStr;
use core::num::ParseIntError;
use core::fmt;
use std::collections::HashMap;

#[derive(Debug)]
struct Quantity {
    material: String,
    quantity: u64,
}
impl FromStr for Quantity {
    type Err = ParseIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.split(" ").collect::<Vec<_>>();
        let quantity = s[0].parse()?;
        Ok(Quantity {
            quantity,
            material: s[1].to_string()
        })
    }
}
impl fmt::Display for Quantity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.material, self.quantity)
    }
}

#[derive(Debug)]
struct Reaction {
    inputs: Vec<Quantity>,
    output: Quantity,
}
impl FromStr for Reaction {
    type Err = ParseIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let io = s.split(" => ").collect::<Vec<_>>();
        let inputs = io[0]
            .split(", ")
            .map(|i| i.parse().unwrap())
            .collect::<Vec<_>>();
        let output = io[1].parse()?;


        Ok(Reaction { inputs, output })
    }
}
impl fmt::Display for Reaction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let input = self.inputs.iter().map(|i| i.to_string()).collect::<Vec<_>>().join(", ");
        write!(f, "{} => {}", input, self.output)
    }
}

struct State {
    ore_cost: u64,
    stockpile: HashMap<String, u64>,
}
impl State {
    fn produce_fuel(amount: u64, recipes: &Vec<Reaction>) -> u64 {
        let mut state = State {
            ore_cost: 0,
            stockpile: HashMap::new(),
        };
        state.produce(&Quantity {material: "FUEL".to_string(), quantity: amount}, &recipes);
        state.ore_cost
    }
    fn produce(&mut self, goal: &Quantity, recipes: &Vec<Reaction>) {
        let recipe = recipes.iter().find(|r| {
            r.output.material == goal.material
        }).expect(&format!("Couldn't find recipe for {}", goal.material));

        let stockpile = self.stockpile.entry(goal.material.to_string()).or_insert(0);
        if *stockpile >= goal.quantity {
            *stockpile -= goal.quantity;
            return;
        }
        let needed = goal.quantity - *stockpile;
        *stockpile = 0;

        let mut times = needed / recipe.output.quantity;
        if needed % recipe.output.quantity > 0 {
            times += 1
        }
        *stockpile = (times * recipe.output.quantity) - needed;
        // println!("Will run <{}> {} times", recipe, times);

        for i in &recipe.inputs {
            if i.material == "ORE" {
                self.ore_cost += i.quantity * times
            } else {
                self.produce(&Quantity {
                    material: i.material.to_string(),
                    quantity: i.quantity * times
                 } , recipes)
            }
        }
    }
}

const A_TRILLION: u64 = 1_000_000_000_000;
fn uses_a_trillion_fuel(amount: u64, recipes: &Vec<Reaction>) -> bool {
    State::produce_fuel(amount, &recipes) > A_TRILLION
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let recipes = buffer.lines()
        .map(|l| l.parse::<Reaction>().unwrap())
        .collect::<Vec<_>>();

    let min_cost = State::produce_fuel(1, &recipes);
    println!("One unit of fuel requires {} ORE", min_cost);

    let mut fuel_to_produce = A_TRILLION / min_cost;
    println!("{} FUEL costs {}", fuel_to_produce, State::produce_fuel(fuel_to_produce, &recipes));

    while !uses_a_trillion_fuel(fuel_to_produce+1000, &recipes) {
        fuel_to_produce += 1000
    }
    while !uses_a_trillion_fuel(fuel_to_produce+100, &recipes) {
        fuel_to_produce += 100
    }
    while !uses_a_trillion_fuel(fuel_to_produce+10, &recipes) {
        fuel_to_produce += 10
    }
    while !uses_a_trillion_fuel(fuel_to_produce+1, &recipes) {
        fuel_to_produce += 1
    }
    println!("{} FUEL costs {}", fuel_to_produce, State::produce_fuel(fuel_to_produce, &recipes));

    Ok(())
}
