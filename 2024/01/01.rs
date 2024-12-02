use std::fs;

fn main() {
    assert_eq!(11, part1("sample.txt".to_string()));

    println!("Part 1: {}", part1("input.txt".to_string()));

    assert_eq!(31, part2("sample.txt".to_string()));

    println!("Part 2: {}", part2("input.txt".to_string()));
}

fn part1(file_path: String) -> i32 {
    read_and_calculate(file_path, calculate_difference)
}

fn part2(file_path: String) -> i32 {
    read_and_calculate(file_path, calculate_similarity)
}

fn read_and_calculate(file_path: String, f: fn(Vec<i32>, Vec<i32>) -> i32) -> i32 {
    let (list1, list2) = read_and_parse_file(file_path);
    
    f(list1, list2)
}

fn calculate_difference(list1: Vec<i32>, list2: Vec<i32>) -> i32 {
    let mut sum = 0;
    for (a, b) in list1.iter().zip(list2.iter()) {
        sum += (b - a).abs()
    }

    sum
}

fn calculate_similarity(list1: Vec<i32>, list2: Vec<i32>) -> i32 {
    let mut sum = 0;

    for a in list1 {
        sum += a * list2.iter().filter(|&&b| b == a).count() as i32;
    }

    sum
}

fn read_and_parse_file(file_path: String) -> (Vec<i32>, Vec<i32>) {
    let contents = fs::read_to_string(file_path)
        .expect("Something went wrong reading the file");

    let lines: Vec<(i32, i32)> = contents.lines()
        .map(|s| {
            let mut iter = s.split_whitespace()
                .map(|c| c.parse::<i32>().expect(&format!("Not a number: {}", c)));
            (iter.next().unwrap(), iter.next().unwrap())
        })
        .collect();

    let mut list1 = Vec::new();
    let mut list2 = Vec::new();

    for (a, b) in lines {
        list1.push(a);
        list2.push(b);
    }

    list1.sort();
    list2.sort();

    (list1, list2)
}