use std::fmt::Debug;
use std::fs;
use std::str::FromStr;

fn main() {
    assert_eq!(480, part1("sample.txt".to_string()));

    println!("Part 1: {}", part1("input.txt".to_string()));

    assert_eq!(875318608908, part2("sample.txt".to_string()));

    println!("Part 2: {}", part2("input.txt".to_string()));
}

#[derive(Debug,Clone)]
struct Machine {
    button_a: (i32, i32),
    button_b: (i32, i32),
    prize: (i64, i64)
}

fn part1(file_path: String) -> i64 {
    let machines = read_and_parse(file_path);
    
    let mut token_sum = 0;
    for machine in machines {
        if let Some(cost) = calculate_cost(
            machine.prize,
            machine.button_a,
            machine.button_b) {
                
            token_sum += cost;
        }
    }
    
    token_sum
}

fn part2(file_path: String) -> i64 {
    let machines = read_and_parse(file_path);

    let mut token_sum = 0;
    for machine in machines {
        if let Some(cost) = calculate_cost(
            (machine.prize.0 + 10000000000000, machine.prize.1 + 10000000000000),
            machine.button_a,
            machine.button_b) {

            token_sum += cost;
        }
    }
    
    token_sum
}

fn calculate_cost(prize: (i64, i64), button_a: (i32, i32), button_b: (i32, i32)) -> Option<i64>  {
    let a = (prize.0 * button_b.1 as i64 - prize.1 * button_b.0 as i64) as f64 / (button_a.0 * button_b.1 - button_a.1 * button_b.0) as f64;
    let b = (prize.0 as f64 - button_a.0 as f64 * a) / button_b.0 as f64;

    if a.floor() as f64 == a && b.floor() as f64 == b && a >= 0.0 && b >= 0.0 {
        Some(a as i64 * 3 + b as i64)
    } else {
        None
    }
}

fn parse_line<T: FromStr<Err = impl Debug> + Copy>(line: String, split_def_on: char) -> (T, T) {
    let def = line.split(": ").last().unwrap();

    let parts = def.split(", ")
        .map(|c| c.split(split_def_on).last().unwrap().parse().unwrap())
        .collect::<Vec<T>>();
    
    (parts[0], parts[1])
}

fn read_and_parse(file_path: String) -> Vec<Machine> {
    let contents = fs::read_to_string(file_path)
        .expect("Something went wrong reading the file");
    
    let lines: Vec<&str> = contents.lines().collect();
    let mut machines = Vec::<Machine>::new();
    let mut i = 0;
    while i < lines.len() {
        machines.push(Machine {
            button_a: parse_line(lines[i].to_string(), '+'),
            button_b: parse_line(lines[i + 1].to_string(), '+'),
            prize: parse_line(lines[i + 2].to_string(), '=')
        });

        i += 4;
    }

    machines
}