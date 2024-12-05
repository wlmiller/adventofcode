use std::cmp::Ordering;
use std::fs;

fn main() {
    assert_eq!(143, part1("sample.txt".to_string()));

    println!("Part 1: {}", part1("input.txt".to_string()));

    assert_eq!(123, part2("sample.txt".to_string()));

    println!("Part 2: {}", part2("input.txt".to_string()));
}

fn part1(file_path: String) -> i32 {
    let (rules, updates) = read_and_parse(file_path);

    let mut sum = 0;
    for update in updates {
        if is_valid(&update, &rules) {
            sum += get_middle(&update);
        }
    }

    sum
}

fn part2(file_path: String) -> i32 {
    let (rules, updates) = read_and_parse(file_path);

    let mut sum = 0;
    let cmp = cmp(&rules);
    for mut update in updates {
        if !is_valid(&update, &rules) {
            update.sort_by(&cmp);
            sum += get_middle(&update);
        }
    }

    sum
}

fn get_middle(update: &Vec<String>) -> i32 {
    update.get(update.len() / 2).unwrap().parse::<i32>().unwrap()
}

fn is_valid(update: &Vec<String>, rules: &Vec<Rule>) -> bool {
    for rule in rules {
        let first = update.iter().position(|x| x == &rule.fst);
        let second = update.iter().position(|x| x == &rule.snd);

        if first.is_none() || second.is_none() {
            continue;
        }

        if first.unwrap() > second.unwrap() {
            return false;
        }
    }

    true
}

fn cmp<'a>(rules: &'a Vec<Rule>) -> impl Fn(&String, &String) -> Ordering + 'a {
    move |a, b| {
        let rule = rules.iter()
            .find(|x| (x.fst == *a && x.snd == *b) || (x.fst == *b && x.snd == *a));
        
        if rule.is_none() {
            return Ordering::Equal;
        } else if rule.unwrap().fst == *a {
            return Ordering::Less;
        } else {
            return Ordering::Greater;
        }
    }
}

fn read_and_parse(file_path: String) -> (Vec<Rule>, Vec<Vec<String>>) {
    let contents = fs::read_to_string(file_path)
        .expect("Something went wrong reading the file");

    let mut rules = Vec::<Rule>::new();
    let mut updates = Vec::<Vec<String>>::new();
    let mut in_rules = true;
    for line in contents.lines() {
        if line == "" {
            in_rules = false;
            continue;
        }

        if in_rules {
            let rule_parts: Vec<&str> = line.split("|").collect();
            let first = rule_parts[0].to_string();
            let second = rule_parts[1].to_string();
            rules.push(Rule { fst: first, snd: second });
        } else {
            updates.push(line.split(",").map(|x| x.to_string()).collect());
        }
    }

    (rules, updates)
}

#[derive(Debug, Clone)]
struct Rule {
    fst: String,
    snd: String
}