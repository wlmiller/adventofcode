use std::collections::HashSet;
use std::fs;

fn main() {
    assert_eq!(22, part1("sample.txt".to_string(), (6, 6), 12));

    println!("Part 1: {}", part1("input.txt".to_string(), (70, 70), 1024));

    assert_eq!("6,1", part2("sample.txt".to_string(), (6, 6), 12));

    println!("Part 2: {}", part2("input.txt".to_string(), (70, 70), 1024));
}

fn part1(file_path: String, dimensions: (i32, i32), byte_count: usize) -> usize {
    let bytes = read_and_parse(file_path);

    let corrupted = bytes[..byte_count].iter().collect::<HashSet<_>>();
    
    find_path(dimensions, &corrupted).unwrap().len() - 1 as usize
}

fn part2(file_path: String, dimensions: (i32, i32), start: usize) -> String {
    let bytes = read_and_parse(file_path);

    let mut path = find_path(dimensions, &bytes[..start].iter().collect()).unwrap();
    
    for i in start + 1..bytes.len() {
        let new_corrupted = bytes[i - 1];

        if path.contains(&new_corrupted) {
            let new_path = find_path(dimensions, &bytes[..i].iter().collect());
            
            if new_path.is_none() {
                return format!("{},{}", new_corrupted.0, new_corrupted.1);
            } else {
                path = new_path.unwrap();
            }
        }
    }
    
    panic!("Not found!");
}

fn find_path(dimensions: (i32, i32), corrupted: &HashSet<&(i32, i32)>) -> Option<Vec<(i32, i32)>> {
    let mut paths = vec![(vec![(0, 0)], 0, heur((0, 0), dimensions))];
    let mut visited = HashSet::new();
    visited.insert((0, 0));
    loop {
        if paths.len() == 0 {
            return None;
        }

        paths.sort_by(|a, b| (a.1 + a.2).cmp(&(b.1 + b.2)));
        let (path, f, _) = paths.remove(0);
        let pos = path.last().unwrap();
        visited.insert(*pos);
        paths = paths.into_iter().filter(|(p, _, _)| p.last().unwrap() != pos).collect();

        if *pos == dimensions {
            return Some(path);
        }

        for dir in &vec![(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let next = (pos.0 + dir.0, pos.1 + dir.1);

            let mut next_path = path.clone();
            next_path.push(next);
            if !visited.contains(&next) && accessible(next, dimensions, &corrupted) {
                paths.push((next_path, f + 1, heur(next, dimensions)));
            }
        }
    }
}

fn accessible(pos: (i32, i32), dimensions: (i32, i32), corrupted: &HashSet<&(i32, i32)>) -> bool {
    pos.0 >= 0 && pos.0 <= dimensions.0 && pos.1 >= 0 && pos.1 <= dimensions.1 && !corrupted.contains(&pos)
}

fn heur(pos: (i32, i32), end: (i32, i32)) -> i32 {
    (pos.0 - end.0).abs() + (pos.1 - end.1).abs()
}

fn read_and_parse(file_path: String) -> Vec<(i32, i32)> {
    let contents = fs::read_to_string(file_path)
        .expect("Something went wrong reading the file");

    contents.lines().map(|line| {
        let parts: Vec<&str> = line.split(",").collect();
        (parts[0].parse().unwrap(), parts[1].parse().unwrap())
    }).collect()
}