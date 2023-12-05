use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn make_maps(data: Vec<String>) -> Vec<(i32, i32, i32)> {
    let mut maps: Vec<(i32, i32, i32)> = data.iter().map(|line| {
        let parts: Vec<i32> = line.split_whitespace().map(|s| s.parse().unwrap()).collect();
        (parts[1], parts[1] + parts[2], parts[0])
    }).collect();
    maps.sort();
    maps
}

fn convert(value: i32, maps: &Vec<(i32, i32, i32)>) -> i32 {
    for map in maps {
        let (src_start, src_end, dest_start) = map;
        if *src_start <= value && value < *src_end {
            return value - src_start + dest_start;
        }
    }
    value
}

fn read_from_file(filename: &Path) -> (Vec<(i32, i32)>, Vec<Vec<String>>) {
    let file = File::open(filename).unwrap();
    let lines: Vec<String> = io::BufReader::new(file).lines().map(|l| l.unwrap()).collect();

    let seeds_line = &lines[0];
    let seeds_str = seeds_line.split(": ").nth(1).unwrap();
    let seeds: Vec<(i32, i32)> = seeds_str.split_whitespace().map(|s| s.parse().unwrap())
                                         .collect::<Vec<i32>>().chunks(2)
                                         .map(|chunk| (chunk[0], chunk[1])).collect();

    let mut stages = Vec::new();
    let mut stage = Vec::new();
    for line in &lines[1..] {
        if line.ends_with(" map:") {
            if !stage.is_empty() {
                stages.push(stage);
                stage = Vec::new();
            }
        } else if !line.is_empty() {
            stage.push(line.clone());
        }
    }
    if !stage.is_empty() {
        stages.push(stage);
    }

    (seeds, stages)
}

fn lowest_location(filename: &str) -> i32 {
    let path = Path::new(filename);
    let (seeds, data) = read_from_file(&path);

    let mut maps_list = Vec::new();
    for stage in data {
        maps_list.push(make_maps(stage));
    }

    let mut min_locations = Vec::new();
    for seed in seeds {
        let mut location = seed.0;
        for maps in &maps_list {
            location = convert(location, maps);
        }
        min_locations.push(location);
    }

    *min_locations.iter().min().unwrap()
}

fn main() {
    let filename = "day5.txt";
    let result = lowest_location(filename);
    println!("Lowest location: {}", result);
}
