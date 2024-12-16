extern crate lazy_static;

use lazy_static::lazy_static;
use std::collections::HashSet;
use std::fs;
use std::sync::Mutex;

lazy_static! {
    static ref DEAD_ENDS: Mutex<HashSet<((usize, usize), Direction, i32)>> = Mutex::new(HashSet::new());
}

fn main() {
    let sample1 = part1("sample1.txt".to_string());
    assert_eq!(7036, sample1);

    let sample2 = part1("sample2.txt".to_string());
    assert_eq!(11048, sample2);

    let answer = part1("input.txt".to_string());
    println!("Part 1: {}", answer);

    assert_eq!(45, part2("sample1.txt".to_string(), sample1));

    assert_eq!(64, part2("sample2.txt".to_string(), sample2));

    println!("Part 2: {}", part2("input.txt".to_string(), answer));
}

#[derive(Debug,Clone,Copy,Eq,Hash,PartialEq)]
enum Direction {
    North,
    South,
    East,
    West
}

fn part1(file_path: String) -> i32 {
    let (start, end, map) = read_and_parse(file_path);

    let mut paths = vec![(start.clone(), Direction::East, 0, heur(start, &Direction::East, end))];
    
    let mut visited = vec![];
    loop {
        let (pos, dir, f, _) = paths.remove(0);
        visited.push((pos.clone(), dir));

        if pos == end {
            return f;
        }

        let next_pos = step(pos, &dir);
        if !map[next_pos.1][next_pos.0] && !visited.contains(&(next_pos, dir)) {
            paths.push((next_pos, dir, f + 1, heur(next_pos, &dir, end)));
        }

        let cw = rotate(&dir, false);
        if !visited.contains(&(pos, cw)) {
            paths.push((pos, cw, f + 1000, heur(pos, &cw, end)));
        }

        let ccw = rotate(&dir, true);
        if !visited.contains(&(pos, ccw)) {
            paths.push((pos, ccw, f + 1000, heur(pos, &ccw, end)));
        }

        paths.sort_by(|a, b| (a.2 + a.3).cmp(&(b.2 + b.3)));
    }
}

fn part2(file_path: String, best_path_length: i32) -> i32 {
    {
        let mut cache = DEAD_ENDS.lock().unwrap();
        cache.clear();
    }

    let (start, end, map) = read_and_parse(file_path);

    find_paths(start, Direction::East, end, &map, best_path_length).unwrap().len() as i32
}

fn find_paths(pos: (usize, usize), dir: Direction, end: (usize, usize), map: &Vec<Vec<bool>>, length: i32) -> Option<HashSet<(usize, usize)>> {
    {
        let cache = DEAD_ENDS.lock().unwrap();
        if cache.contains(&(pos, dir, length)) {
            return None;
        }
    }
    
    let mut paths = HashSet::new();
    paths.insert(pos);
    if length == 0 && pos == end {   
        return Some(paths);
    } else if length < 0 || heur(pos, &dir, end) > length {
        {
            let mut cache = DEAD_ENDS.lock().unwrap();
            cache.insert((pos, dir, length));
        }
        return None;
    }

    let next_pos = step(pos, &dir);
    let mut paths_exist = false;
    if !map[next_pos.1][next_pos.0] {
        if let Some(next_paths) = find_paths(next_pos, dir, end, map, length - 1) {
            paths_exist = true;
            paths.extend(next_paths);
        }
    }

    if let Some(next_paths) = find_paths(pos, rotate(&dir, false), end, map, length - 1000) {
        paths_exist = true;
        paths.extend(next_paths);
    }

    if let Some(next_paths) = find_paths(pos, rotate(&dir, true), end, map, length - 1000) {
        paths_exist = true;
        paths.extend(next_paths);
    }

    if paths_exist {
        Some(paths)
    } else {
        {
            let mut cache = DEAD_ENDS.lock().unwrap();
            cache.insert((pos, dir, length));
        }
        None
    }
}

fn rotate(dir: &Direction, ccw: bool) -> Direction {
    match &dir {
        Direction::North => if ccw { Direction::West } else { Direction::East },
        Direction::South => if ccw { Direction::East } else { Direction::West },
        Direction::East => if ccw { Direction::North } else { Direction::South },
        Direction::West => if ccw { Direction::South } else { Direction::North }
    }
}

fn step(pos: (usize, usize), dir: &Direction) -> (usize, usize) {
    let dir_step = match &dir {
        Direction::North => (0, -1),
        Direction::South => (0, 1),
        Direction::East => (1, 0),
        Direction::West => (-1, 0)
    };

    ((pos.0 as i32 + dir_step.0) as usize, (pos.1 as i32 + dir_step.1) as usize)
}

fn heur(pos: (usize, usize), dir: &Direction, end: (usize, usize)) -> i32 {
    let dx = end.0 as i32 - pos.0 as i32;
    let dy = end.1 as i32 - pos.1 as i32;

    let rotations = match &dir {
        Direction::North => if dx != 0 { 1 } else if dy > 0 { 2 } else { 0 },
        Direction::South => if dx != 0 { 1 } else if dy < 0 { 2 } else { 0 },
        Direction::East => if dy != 0 { 1 } else if dx < 0 { 2 } else { 0 },
        Direction::West => if dy != 0 { 1 } else if dx > 0 { 2 } else { 0 }
    };

    (dx.abs() + dy.abs() + 1000 * rotations) as i32
}

fn read_and_parse(file_path: String) -> ((usize, usize), (usize, usize), Vec<Vec<bool>>) {
    let contents = fs::read_to_string(file_path)
        .expect("Something went wrong reading the file");

    let mut map = vec![];
    let mut start = (0, 0);
    let mut end = (0, 0);
    for (y, line) in contents.lines().enumerate() {
        let mut row = vec![];
        for (x, c) in line.chars().enumerate() {
            row.push(c == '#');

            if c == 'S' { start = (x, y); }
            if c == 'E' { end = (x, y); }
        }
        map.push(row);
    }

    (start, end, map)
}