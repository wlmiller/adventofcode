use std::fs;

fn main() {
    assert_eq!(3749, part1("sample.txt".to_string()));

    println!("Part 1: {}", part1("input.txt".to_string()));

    assert_eq!(11387, part2("sample.txt".to_string()));

    println!("Part 2: {}", part2("input.txt".to_string()));
}

fn part1(file_path: String) -> i64 {
    get_calibration_result(file_path, false)
}

fn part2(file_path: String) -> i64 {
    get_calibration_result(file_path, true)
}

fn get_calibration_result(file_path: String, include_concat: bool) -> i64 {
    let equations = read_and_parse(file_path);

    let mut sum = 0;

    for (test, nums) in &equations {
        let combinations = get_possible_ops((nums.len() - 1) as i32, include_concat);

        for ops in combinations {
            let mut val = nums[0];
            for (i, op) in ops.into_iter().enumerate() {
                val = op(val, nums[i + 1]);
            }

            if val == *test {
                sum += test;
                break;
            }
        }
        
    }

    sum
}

fn get_possible_ops(count: i32, include_concat: bool) -> Vec<Vec<fn(i64, i64) -> i64>> {
    let mut ops = vec![
        |x, y| x + y,
        |x, y| x * y
    ];

    if include_concat {
        ops.push(|x, y| format!("{}{}", x, y).parse::<i64>().unwrap());
    }

    let mut combinations = ops.clone().into_iter().map(|x| vec![x]).collect::<Vec<Vec<fn(i64, i64) -> i64>>>();
    for _ in 1..count {
        let mut new_combs = Vec::new();
        for c in combinations {
            for op in &ops {
                let mut new_comb = c.clone();
                new_comb.push(op.clone());
                new_combs.push(new_comb);
            }
        }

        combinations = new_combs;
    }

    combinations
}

fn read_and_parse(file_path: String) -> Vec<(i64, Vec<i64>)> {
    let contents = fs::read_to_string(file_path)
        .expect("Something went wrong reading the file");

    let mut equations = Vec::new();

    for line in contents.lines() {
        let split = line.split(": ").collect::<Vec<&str>>();
        let test = split[0].parse::<i64>().unwrap();

        let nums = split[1].split(" ").map(|x| x.parse::<i64>().unwrap()).collect::<Vec<i64>>();
        
        
        equations.push((test, nums));
    }

    equations
}