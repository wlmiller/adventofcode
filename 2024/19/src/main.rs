extern crate lazy_static;

use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fs;
use std::sync::Mutex;

lazy_static! {
    static ref CACHE: Mutex<HashMap<String, i64>> = Mutex::new(HashMap::new());
}

fn main() {
    assert_eq!(6, part1("sample.txt".to_string()));

    println!("Part 1: {}", part1("input.txt".to_string()));

    assert_eq!(16, part2("sample.txt".to_string()));

    println!("Part 2: {}", part2("input.txt".to_string()));
}

fn part1(file_path: String) -> i32 {
    clear_cache();
    let (towels, designs) = read_and_parse(file_path);

    let mut count = 0;
    for design in designs {
        if count_arrangements(design, &towels) > 0 {
            count += 1;
        }
    }

    count
}

fn part2(file_path: String) -> i64 {
    clear_cache();
    let (towels, designs) = read_and_parse(file_path);

    let mut sum = 0;
    for design in designs {
        sum += count_arrangements(design, &towels);
    }

    sum
}

fn clear_cache() {
    {
        let mut cache = CACHE.lock().unwrap();
        cache.clear()
    }
}

fn count_arrangements(design: String, towels: &Vec<String>) -> i64 {
    if design == "" {
        return 1;
    }

    {
        let cache = CACHE.lock().unwrap();
        if cache.contains_key(&design) {
            return cache[&design];
        }
    }

    let mut count = 0;
    for towel in towels {
        if design.starts_with(towel) {
            let new_design = design[towel.len()..].to_string();
            count += count_arrangements(new_design, towels);
        }
    }

    {
        let mut cache = CACHE.lock().unwrap();
        cache.insert(design, count);
    }

    count
}

fn read_and_parse(file_path: String) -> (Vec<String>, Vec<String>) {
    let contents = fs::read_to_string(file_path)
        .expect("Something went wrong reading the file");

    let lines: Vec<&str> = contents.lines().collect();

    let towels = lines[0].split(", ").map(|x| x.to_string()).collect();
    let designs = lines[2..].iter().map(|x| x.to_string()).collect();

    (towels, designs)
}