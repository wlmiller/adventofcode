use std::fs;

fn main() {
    assert_eq!(2, part1("sample.txt".to_string()));

    println!("Part 1: {}", part1("input.txt".to_string()));

    assert_eq!(4, part2("sample.txt".to_string()));

    println!("Part 2: {}", part2("input.txt".to_string()));
}

fn part1(file_path: String) -> i32 {
    let reports = read_and_parse_file(file_path);

    let mut safe = 0;
    
    for report in reports {
        if is_safe(&report) {
            safe += 1;
        }
    }

    safe
}

fn part2(file_path: String) -> i32 {
    let reports = read_and_parse_file(file_path);

    let mut safe = 0;
    
    for report in reports {
        if is_safe(&report) {
            safe += 1;
        } else {
            for sub_report in generate_sub_reports(&report) {
                if is_safe(&sub_report) {
                    safe += 1;
                    break;
                }
            }
        }
    }

    safe
}

fn generate_sub_reports(report: &[i32]) -> Vec<Vec<i32>> {
    let mut sub_reports = Vec::new();
    for i in 0..report.len() {
        let mut sub_report = report.to_vec();
        sub_report.remove(i);
        sub_reports.push(sub_report);
    }
    
    sub_reports
}

fn is_safe(report: &[i32]) -> bool {
    let mut is_safe: bool = true;
    let mut increasing: Option<bool> = None;

    for (i, &value) in report.iter().enumerate() {
        if i == 0 {
            continue;
        }

        let prev = report[i - 1];

        if increasing.is_none() {
            if value > prev {
                increasing = Some(true);
            } else if value < report[i - 1] {
                increasing = Some(false);
            } else {
                is_safe = false;
                break;
            }
        }
        
        if increasing.unwrap() && (value < prev + 1 || value > prev + 3) {
            is_safe = false;
            break;
        } else if !increasing.unwrap() && (value > prev - 1 || value < prev - 3) {
            is_safe = false;
            break;
        }
    }

    is_safe
}

fn read_and_parse_file(file_path: String) -> Vec<Vec<i32>> {
    let contents = fs::read_to_string(file_path)
        .expect("Something went wrong reading the file");

    contents.lines()
        .map(|s| s.split_whitespace()
                .map(|c| c.parse::<i32>().expect(&format!("Not a number: {}", c)))
                .collect())
        .collect()
}