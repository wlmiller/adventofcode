use std::fs;

fn main() {
    assert_eq!(161, part1("sample1.txt".to_string()));

    println!("Part 1: {}", part1("input.txt".to_string()));

    assert_eq!(48, part2("sample2.txt".to_string()));

    println!("Part 2: {}", part2("input.txt".to_string()));
}

fn part1(file_path: String) -> i32 {
    parse_and_execute(file_path, false)
}

fn part2(file_path: String) -> i32 {
    parse_and_execute(file_path, true)
}

fn parse_and_execute(file_path: String, conditionals: bool) -> i32 {
    let program = read_file(file_path);

    let mut in_statement = false;
    let mut statement: Vec<char> = Vec::new();
    let mut sum = 0;
    let mut enabled = true;
    for (i, c) in program.chars().enumerate() {
        if c == 'm' || c == 'd' {
            in_statement = true;
            statement = vec![c];
        } else if i > 0 && in_statement && is_valid_next_char(program.chars().nth(i - 1).unwrap(), c, conditionals) {
            statement.push(c);
            if c == ')' {
                if enabled && statement[0] == 'm' &&  statement.contains(&',') {
                    sum += execute_statement(&statement);
                } else if conditionals {
                    let statement_str: String = statement.iter().collect();
                    if statement_str == "do()" { enabled = true; }
                    else if statement_str == "don't()" { enabled = false; }
                }
                in_statement = false;
            }
        } else {
            in_statement = false;
        }
    }

    sum
}

fn execute_statement(statement: &[char]) -> i32 {
    let nums_str: String = statement[4..statement.len() - 1].iter().collect();
    let nums: Vec<&str> = nums_str.split(',').collect();
    
    nums[0].parse::<i32>().unwrap() * nums[1].parse::<i32>().unwrap()
}

fn is_valid_next_char(prev_char: char, next_char: char, conditionals: bool) -> bool {
    match prev_char {
        'm' => next_char == 'u',
        'u' => next_char == 'l',
        'l' => next_char == '(',
        '(' => next_char.is_digit(10) || (conditionals && next_char == ')'),
        _ if prev_char.is_digit(10) => next_char.is_digit(10) || next_char == ',' || next_char == ')',
        ',' => next_char.is_digit(10),
        'd' => next_char == 'o',
        'o' => next_char == 'n' || next_char == '(',
        'n' => next_char == '\'',
        '\'' => next_char == 't',
        't' => next_char == '(',
        _ => false
    }
}

fn read_file(file_path: String) -> String {
    fs::read_to_string(file_path)
        .expect("Something went wrong reading the file")
}