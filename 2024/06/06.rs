use std::fs;
use std::collections::HashSet;

fn main() {
    assert_eq!(41, part1("sample.txt".to_string()));

    println!("Part 1: {}", part1("input.txt".to_string()));

    assert_eq!(6, part2("sample.txt".to_string()));

    println!("Part 2: {}", part2("input.txt".to_string()));
}

#[derive(Debug)]
enum Path {
    Route(Vec<Guard>),
    Loop
}

impl Path {
    fn unwrap(self: &Path) -> Vec<Guard> {
        match self {
            Path::Route(val) => val.clone(),
            _ => panic!("Cannot unwrap Loop")
        }
    }
}

#[derive(Debug, Clone)]
struct Pos {
    obs: bool,
    visited: bool
}

impl Pos {
    fn new(obs: bool) -> Pos {
        Pos { obs, visited: false }
    }
}

#[derive(Debug, Clone)]
struct Guard {
    pos: (i32, i32),
    dir: (i32, i32)
}

impl Guard {
    fn turn(self: &mut Guard) {
        self.dir = match self.dir {
            (0, -1) => (1, 0),
            (1, 0) => (0, 1),
            (0, 1) => (-1, 0),
            (-1, 0) => (0, -1),
            _ => unreachable!()
        }
    }

    fn is_blocked(self: &Guard, grid: &Vec<Vec<Pos>>) -> bool {
        let new_x = self.pos.0 + self.dir.0;
        let new_y = self.pos.1 + self.dir.1;

        if !in_bounds(grid, (new_x, new_y)) {
            return false;
        }

        grid[(self.pos.1 + self.dir.1) as usize][(self.pos.0 + self.dir.0) as usize].obs
    }

    fn step(self: &mut Guard) {
        self.pos.0 += self.dir.0;
        self.pos.1 += self.dir.1;
    }
}

fn part1(file_path: String) -> i32 {
    let (grid, guard) = read_and_parse(file_path);

    let visited = patrol(grid, guard).unwrap();

    dedupe_by_pos(visited).len() as i32
}

fn part2(file_path: String) -> i32 {
    let (grid, guard) = read_and_parse(file_path);

    // Always use the first visit to a potential position
    let visited = dedupe_by_pos(patrol(grid.clone(), guard.clone()).unwrap());

    let mut loop_count = 0;
    for (i, g) in visited.iter().enumerate() {
        if i == 0 {
            continue;
        }

        let mut grid = grid.clone();
        grid[g.pos.1 as usize][g.pos.0 as usize].obs = true;

        // Start the patrol from the previous visited position - the route will be unaffected up to that point
        let guard =  visited[i-1].clone();

        match patrol(grid, guard.clone()) {
            Path::Loop => loop_count += 1,
            _ => ()
        }
    }

    loop_count
}

fn patrol(grid: Vec<Vec<Pos>>, guard: Guard) -> Path {
    let mut grid = grid.clone();
    let mut guard = guard.clone();

    let mut visited = Vec::<Guard>::new();
    while in_bounds(&grid, guard.pos) {
        let pos = &mut grid[guard.pos.1 as usize][guard.pos.0 as usize];
    
        pos.visited = true;

        if !visited.iter().any(|g| &g.pos == &guard.pos && &g.dir == &guard.dir) {
            visited.push(guard.clone());
        } else {
            return Path::Loop;
        }

        if Guard::is_blocked(&guard, &grid) {
            Guard::turn(&mut guard);
        } else {
            Guard::step(&mut guard);
        }
    }

    Path::Route(visited)
}

fn dedupe_by_pos(vec: Vec<Guard>) -> Vec<Guard>{
    let mut seen = HashSet::new();
    vec.into_iter().filter(|g| seen.insert(g.pos)).collect()
}

fn in_bounds(grid: &Vec<Vec<Pos>>, pos: (i32, i32)) -> bool {
    pos.0 >= 0 && pos.0 < grid[0].len() as i32 && pos.1 >= 0 && pos.1 < grid.len() as i32
}

fn read_and_parse(file_path: String) -> (Vec<Vec<Pos>>, Guard) {
    let contents = fs::read_to_string(file_path).unwrap();
    let mut grid = Vec::<Vec<Pos>>::new();
    let mut guard: Option<Guard> = None;
    for line in contents.lines() {
        let mut row = Vec::<Pos>::new();
        for c in line.chars() {
            match c {
                '#' => row.push(Pos::new(true)),
                '.' => row.push(Pos::new(false)),
                '^' | 'v' | '<' | '>' => {
                    guard = Some(Guard {
                        pos: (row.len() as i32, grid.len()  as i32),
                        dir: match c {
                            '^' => (0, -1),
                            'v' => (0, 1),
                            '<' => (-1, 0),
                            '>' => (1, 0),
                            _ => unreachable!()
                        }
                    });
                    row.push(Pos::new(false));
                }
                _ => panic!("Invalid character in input")   
            }
        }
        grid.push(row);
    }

    (grid, guard.unwrap())
}

