use std::collections::HashSet;
use std::fs;

fn main() {
    assert_eq!(12, part1("sample.txt".to_string(), 11, 7));

    println!("Part 1: {}", part1("input.txt".to_string(), 101, 103));

    println!("Part 2: ");
    part2("input.txt".to_string(), 101, 103);
}

#[derive(Debug,Clone)]
struct Robot {
    pos: (i32, i32),
    vel: (i32, i32)
}

impl Robot {
    fn move_times(&mut self, width: i32, height: i32, steps: i32) {
        self.pos.0 = (self.pos.0 + self.vel.0 * steps).rem_euclid(width);
        self.pos.1 = (self.pos.1 + self.vel.1 * steps).rem_euclid(height);
    }
}

fn part1(file_path: String, width: i32, height: i32) -> i32 {
    let mut robots = read_and_parse(file_path);

    for robot in &mut robots {
        robot.move_times(width, height, 100);
    }

    score(robots, width, height)
}

fn part2(file_path: String, width: i32, height: i32) {
    let mut robots = read_and_parse(file_path);

    for i in 1..10_000 {
        for robot in &mut robots {
            robot.move_times(width, height, 1);
        }

        let distinct_positions: HashSet<(i32, i32)> = robots.iter().map(|r| r.pos).collect();
        if distinct_positions.len() == robots.len() {
            println!("======================= {} =======================", i);
            print(robots.clone(), width, height);
        }
    }
}

fn score(robots: Vec<Robot>, width: i32, height: i32) -> i32 {
    let mut quadrants = (0, 0, 0, 0);

    for robot in robots {
        if robot.pos.0 < width / 2 && robot.pos.1 < height / 2 {
            quadrants.0 += 1;
        } else if robot.pos.0 > width / 2 && robot.pos.1 < height / 2 {
            quadrants.1 += 1;
        } else if robot.pos.0 < width / 2 && robot.pos.1 > height / 2 {
            quadrants.2 += 1;
        } else if robot.pos.0 > width / 2 && robot.pos.1 > height / 2 {
            quadrants.3 += 1;
        }
    }

    quadrants.0 * quadrants.1 * quadrants.2 * quadrants.3
}

fn print(robots: Vec<Robot>, width: i32, height: i32) {
    let mut grid = vec![vec![0; width as usize]; height as usize];

    for robot in robots {
        grid[robot.pos.1 as usize][robot.pos.0 as usize] += 1;
    }

    for row in grid {
        println!("{}", row.iter().map(|x| if *x > 0 { x.to_string() } else { " ".to_string() }).collect::<Vec<String>>().join(""));
    }

    println!();
}

fn read_and_parse(file_path: String) -> Vec<Robot> {
    let contents = fs::read_to_string(file_path)
        .expect("Something went wrong reading the file");

    let mut robots = Vec::new();
    for line in contents.lines() {
        let parts: Vec<&str> = line.split(" ").collect();
        let pos_parts: Vec<&str> = parts[0].split("=").collect();
        let vel_parts: Vec<&str> = parts[1].split("=").collect();
        let pos: Vec<&str> = pos_parts[1].split(",").collect();
        let vel: Vec<&str> = vel_parts[1].split(",").collect();
        robots.push(Robot {
            pos: (pos[0].parse().unwrap(), pos[1].parse().unwrap()),
            vel: (vel[0].parse().unwrap(), vel[1].parse().unwrap())
        });
    }

    robots
}