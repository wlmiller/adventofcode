use std::fs;

fn main() {
    assert_eq!(36, part1("sample.txt".to_string()));

    println!("Part 1: {}", part1("input.txt".to_string()));

    assert_eq!(81, part2("sample.txt".to_string()));

    println!("Part 2: {}", part2("input.txt".to_string()));
}

fn part1(file_path: String) -> i32 {
    get_trailhead_sum(file_path, false)
}

fn part2(file_path: String) -> i32 {
    get_trailhead_sum(file_path, true)
}

fn get_trailhead_sum(file_path: String, rating: bool) -> i32 {
    let map = read_and_parse(file_path);

    let mut sum = 0;
    for (i, row) in map.iter().enumerate() {
        for (j, cell) in row.iter().enumerate() {
            if *cell == 0 {
                sum += find_trail_ends((i as i32, j as i32), &map, rating).len() as i32;
            }
        }
    }

    sum
}

fn find_trail_ends(trailhead: (i32, i32), map: &Vec<Vec<i32>>, distinct_paths: bool) -> Vec<(i32, i32)> {
    let mut trails = vec![trailhead];

    for val in 1..10 {
        let mut new_trails = Vec::new();

        for pos in &trails {
            for dir in vec![(1, 0), (0, 1), (-1, 0), (0, -1)] {
                let new_pos = (pos.0 + dir.0, pos.1 + dir.1);
                if !in_bounds(map, new_pos) { continue; }

                if map[new_pos.0 as usize][new_pos.1 as usize] == val {
                    if distinct_paths || !new_trails.contains(&new_pos) {
                        new_trails.push(new_pos);
                    }
                }
            }
        }

        trails = new_trails;
    }

    trails
}

fn in_bounds(map: &Vec<Vec<i32>>, pos: (i32, i32)) -> bool {
    pos.0 >= 0 && pos.0 < map.len() as i32 && pos.1 >= 0 && pos.1 < map[0].len() as i32
}

fn read_and_parse(file_path: String) -> Vec<Vec<i32>> {
    let contents = fs::read_to_string(file_path)
        .expect("Something went wrong reading the file");

    contents.lines()
        .map(|line|
            line.chars()
                .map(|x| x.to_digit(10).unwrap() as i32)
                .collect()
        )
        .collect()
}