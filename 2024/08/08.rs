use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;


fn main() {
    assert_eq!(14, part1("sample.txt".to_string()));

    println!("Part 1: {}", part1("input.txt".to_string()));

    assert_eq!(34, part2("sample.txt".to_string()));

    println!("Part 2: {}", part2("input.txt".to_string()));
}

fn part1(file_path: String) -> i32 {
    let (size, antennae) = read_and_parse(file_path);

    let mut antinodes: Vec<(i32, i32)> = Vec::new();
    for (_, coords) in &antennae {
        for (i, (x1, y1)) in coords.into_iter().enumerate() {
            for (x2, y2) in coords.into_iter().skip(i + 1) {
                antinodes.push((x1 - 2*(x1 - x2), y1 - 2*(y1 - y2)));
                antinodes.push((x2 - 2*(x2 - x1), y2 - 2*(y2 - y1)));
            }
        }
    }

    count_antinodes(antinodes, size)
}

fn part2(file_path: String) -> i32 {
    let (size, antennae) = read_and_parse(file_path);

    let mut antinodes: Vec<(i32, i32)> = Vec::new();
    for (_, coords) in &antennae {
        for (i, (x1, y1)) in coords.into_iter().enumerate() {
            for (x2, y2) in coords.into_iter().skip(i + 1) {
                let dx = x2 - x1;
                let dy = y2 - y1;

                let mut i = 0;
                loop {
                    let pos1 = (x1 - i*dx, y1 - i*dy);
                    let pos2 = (x2 + i*dx, y2 + i*dy);

                    if !in_bounds(size, pos1) && !in_bounds(size, pos2) {
                        break;
                    }

                    antinodes.push(pos1);
                    antinodes.push(pos2);
                    i += 1;
                }
            }
        }
    }

    count_antinodes(antinodes, size)
}

fn in_bounds(size: (i32, i32), pos: (i32, i32)) -> bool {
    pos.0 >= 0 && pos.0 < size.0 && pos.1 >= 0 && pos.1 < size.1
}

fn count_antinodes(antinodes: Vec<(i32, i32)>, size: (i32, i32)) -> i32 {
    antinodes.into_iter()
        .filter(|(x, y)| *x >= 0 && *x < size.0 && *y >= 0 && *y < size.1)
        .collect::<HashSet<(i32, i32)>>()
        .len() as i32
}

fn read_and_parse(file_path: String) -> ((i32, i32), HashMap<char, Vec<(i32, i32)>>) {
    let contents = fs::read_to_string(file_path)
        .expect("Something went wrong reading the file");

    let mut antennae: HashMap<char, Vec<(i32, i32)>> = HashMap::new();
    let mut x_size = 0;
    let mut y_size = 0;
    for (y, line) in contents.lines().into_iter().enumerate() {
        x_size = line.len() as i32;
        y_size += 1;
        for (x, c) in line.chars().enumerate() {
            if c == '.' { continue; }

            if let Some(ant) = antennae.get_mut(&c) {
                ant.push((x as i32, y as i32));
            } else {
                antennae.insert(c, vec![(x as i32, y as i32)]);
            }
        }
    }

    ((x_size, y_size), antennae)
}