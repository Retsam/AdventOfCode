use std::io::{self, Read};
use std::collections::HashMap;

type ObjId = String;
type Orbit = (ObjId, ObjId);

#[derive(Debug)]
struct OrbitInfo {
    primary: Option<ObjId>,
    satellites: Vec<ObjId>,
}
impl OrbitInfo {
    fn new() -> OrbitInfo {
        OrbitInfo {
            primary: None,
            satellites: vec!()
        }
    }
}
type OrbitMap = HashMap<ObjId, OrbitInfo>;

fn build_orbit_info(orbits: Vec<Orbit>) -> OrbitMap {
    let mut orbit_map = HashMap::new();
    for (primary, satellite) in orbits {
        let primary_info = orbit_map.entry(primary.to_string()).or_insert(OrbitInfo::new());
        primary_info.satellites.push(satellite.to_string());

        let mut sat_info = orbit_map.entry(satellite).or_insert(OrbitInfo::new());
        sat_info.primary = Some(primary);
    }

    orbit_map
}

fn count_orbits(orbit_map: &OrbitMap) -> u32 {
    _count_orbits(orbit_map, &"COM".to_owned(), 0)
}
fn _count_orbits(orbit_map: &OrbitMap, obj_id: &String, depth: u32) -> u32 {
    let info = &orbit_map[obj_id];
    let count = info.satellites.iter().map(|sat_id| {
        _count_orbits(&orbit_map, sat_id, depth + 1)
    }).sum::<u32>();
    depth + count
}

fn shortest_transfer(orbit_map: &OrbitMap, obj1: &str, obj2: &str) -> usize {
    let mut ancestors = vec!(obj1);
    let mut o = obj1;
    let get_parent = |o| {
        &orbit_map[o].primary
    };
    loop {
        match get_parent(o) {
            None => break,
            Some(oo) => {
                ancestors.push(&oo);
                o = &oo;
            },
        }
    };
    o = obj2;
    let mut dist = 0;
    loop {
        if let Some(idx) = ancestors.iter().position(|&x| x == o) {
            break dist + idx;
        }
        match get_parent(o) {
            None => panic!("Ran out of parents without finding ancestor"),
            Some(oo) => {
                dist += 1;
                o = &oo;
            },
        }
    }
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let orbits = buffer.lines().map(|line| {
        let split = line.split(")").collect::<Vec<_>>();
        (split[0].to_string(), split[1].to_string())
    }).collect::<Vec<_>>();


    let orbit_info = build_orbit_info(orbits);
    println!("Orbit checksum is {:#?}", count_orbits(&orbit_info));
    // Minus 2, because the distance doesn't include "YOU" or "SAN"
    println!("Minimum transfer distance is {}", shortest_transfer(&orbit_info, "YOU", "SAN") - 2);
    Ok(())
}
