use std::collections::{BinaryHeap, HashMap};
use std::io::{self, Read};

#[derive(Debug)]
enum Cmd {
    Cd(String),
    Ls,
}
impl TryFrom<&str> for Cmd {
    type Error = String;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value == "$ ls" {
            Ok(Cmd::Ls)
        } else if let Some(dir) = value.strip_prefix("$ cd ") {
            Ok(Cmd::Cd(dir.to_string()))
        } else {
            Err(format!("Not a command: {}", value))
        }
    }
}

#[derive(Debug)]
enum Output {
    File(u32, String),
    Dir(String),
}

impl TryFrom<&str> for Output {
    type Error = String;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if let Some(dir) = value.strip_prefix("dir ") {
            return Ok(Output::Dir(dir.to_string()));
        }
        let mut parts = value.split(' ');
        if let (Some(size), Some(name), None) = (
            parts.next().and_then(|x| x.parse::<u32>().ok()),
            parts.next(),
            parts.next(),
        ) {
            Ok(Output::File(size, name.to_string()))
        } else {
            Err(format!("Not output: {}", value))
        }
    }
}

struct Path(Vec<String>);

impl Path {
    fn new() -> Path {
        Path(Vec::new())
    }
    fn cd(&mut self, dir: String) {
        if dir == ".." {
            self.0.pop();
            return;
        }
        let prefix = self
            .0
            .last()
            .map(|parent| {
                // don't double prefix /
                if parent == "/" {
                    "".to_owned()
                } else {
                    parent.to_owned() + "/"
                }
            })
            // empty path, no prefix
            .unwrap_or_else(|| "".to_owned());
        self.0.push(prefix + &dir);
    }
}

fn read_input() -> io::Result<String> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    Ok(buffer)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = read_input()?;

    let mut path = Path::new();
    let mut sizes: HashMap<String, u32> = HashMap::new();

    let mut lines = input.lines();
    // We manually advance the lines iterator using next_cmd because the ls command will over-read to find the end of ls output
    let mut next_cmd = lines.next();
    while let Some(cmd) = next_cmd {
        match TryInto::<Cmd>::try_into(cmd)? {
            Cmd::Cd(dir) => {
                path.cd(dir);
                // manually advance the iterator to match ls behavior
                next_cmd = lines.next();
            }
            Cmd::Ls => {
                while let Some(out) = {
                    next_cmd = lines.next();
                    next_cmd.and_then(|x| TryInto::<Output>::try_into(x).ok())
                } {
                    if let Output::File(size, _) = out {
                        for dir in &path.0 {
                            sizes
                                .entry(dir.to_string())
                                .and_modify(|x| *x += size)
                                .or_insert(size);
                        }
                    }
                }
            }
        }
    }

    let p1: u32 = sizes.values().filter(|x| **x <= 100000).sum();

    let used_size = sizes.get("/").expect("Expected a root size");
    let space_to_free = used_size - 40000000; // 70K available, need 30K free

    let sorted_sizes = sizes
        .into_values()
        .collect::<BinaryHeap<_>>()
        .into_sorted_vec();
    let p2 = sorted_sizes
        .into_iter()
        .find(|size| *size > space_to_free)
        .expect("No dir was big enough");

    println!("Part 1: {}\nPart 2: {}", p1, p2);

    Ok(())
}
