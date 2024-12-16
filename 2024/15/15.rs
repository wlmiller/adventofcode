use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;

fn main() {
    assert_eq!(2028, part1("sample1.txt".to_string()));

    assert_eq!(10092, part1("sample2.txt".to_string()));

    println!("Part 1: {}", part1("input.txt".to_string()));

    assert_eq!(9021, part2("sample2.txt".to_string()));

    println!("Part 2: {}", part2("input.txt".to_string()));
}

#[derive(Debug,Clone,PartialEq)]
enum Object {
    Robot,
    Box(BoxPart),
    Wall
}

#[derive(Debug,Clone,PartialEq)]
enum BoxPart {
    Single,
    Left,
    Right
}

#[derive(Debug,Clone, PartialEq)]
enum Direction {
    Up,
    Down,
    Left,
    Right
}

#[derive(Debug,Clone)]
struct Map {
    inner: HashMap<(i32, i32), Object>
}

impl Map {
    fn next(&self, from: (i32, i32), direction: &Direction) -> (i32, i32) {
        let dir = match direction {
            Direction::Up => (0, -1),
            Direction::Down => (0, 1),
            Direction::Left => (-1, 0),
            Direction::Right => (1, 0)
        };

        (from.0 + dir.0, from.1 + dir.1)
    }

    fn move_object(&mut self, from: (i32, i32), dir: &Direction) -> (i32, i32) {
        let obj = self.inner.remove(&from).unwrap();
        let to = self.next(from, dir);

        if self.inner.get(&to).is_some() {
            panic!("Conflicting move!");
        }

        self.inner.insert(to, obj);

        to
    }

    fn get_pushed_objs(&self, from: (i32, i32), dir: &Direction) -> Option<Vec<(i32, i32)>> {
        let mut to_push = vec![];
        let next = self.next(from, dir);

         if let Some(Object::Wall) = self.inner.get(&next) {
            return None;
        } else if self.inner.get(&next).is_none() {
            return Some(to_push);
        }

        if self.inner.get(&next).unwrap().eq(&Object::Box(BoxPart::Single))
            || (matches!(self.inner.get(&next), Some(Object::Box(_))) && (*dir == Direction::Left || *dir == Direction::Right)) {

            to_push.push(next);
            let next_to_push = self.get_pushed_objs(next, dir);
            if next_to_push.is_none() {
                return None;
            } else {
                to_push.extend(next_to_push.unwrap());
            }
        } else if let Some(Object::Box(part)) = self.inner.get(&next) {
            let neightbor = if *part == BoxPart::Left {
                (next.0+1, next.1)
            } else {
                (next.0-1, next.1)
            };

            let boxes = vec![next, neightbor];
            for box_pos in boxes {
                to_push.push(box_pos);
                let next_to_push = self.get_pushed_objs(box_pos, dir);
                if next_to_push.is_none() {
                    return None;
                } else {
                    to_push.extend(next_to_push.unwrap());
                }
            }
        }

        Some(to_push)
    }

    fn sort_moves(&self, positions: Vec<&(i32, i32)>, direction: &Direction) -> Vec<(i32, i32)> {
        let mut sorted: Vec<(i32, i32)> = positions.into_iter()
            .map(|p| p.clone())
            .collect::<HashSet<(i32, i32)>>()
            .into_iter().collect();
        
        sorted.sort_by(|a, b| match &direction {
            Direction::Up => a.1.cmp(&b.1),
            Direction::Down => a.1.cmp(&b.1).reverse(),
            Direction::Left => a.0.cmp(&b.0),
            Direction::Right => a.0.cmp(&b.0).reverse()
        });
        sorted
    }

    fn move_robot(&mut self, from: (i32, i32), direction: Direction) -> (i32, i32) {
        if !self.inner.get(&from).unwrap().eq(&Object::Robot) {
            panic!("Invalid move");
        }

        let to_move = self.get_pushed_objs(from, &direction);
        
        if to_move.is_none() {
            return from;
        }

        let mut to = from;
        let binding = to_move.unwrap();
        let mut to_move: Vec<&(i32, i32)> = binding.iter().collect();
        to_move.push(&from);
        for pos in self.sort_moves(to_move, &direction) {
            to = self.move_object(pos, &direction);
        }

        to
    }
}

fn part1(file_path: String) -> i32 {
    let (robot_pos, mut map, moves) = read_and_parse(file_path);

    get_score(robot_pos, &mut map, moves)
}

fn part2(file_path: String) -> i32 {
    let (mut robot_pos, mut map, moves) = read_and_parse(file_path);
    map = resize_map(map);
    robot_pos.0 *= 2;

    get_score(robot_pos, &mut map, moves)
}

fn get_score(mut robot_pos: (i32, i32), map: &mut Map, moves: Vec<Direction>) -> i32 {
    for dir in moves {
        robot_pos = map.move_robot(robot_pos, dir);
    }

    let mut score = 0;
    for (pos, obj) in map.inner.iter() {
        if let Object::Box(part) = obj {
            if part == &BoxPart::Single || part == &BoxPart::Left {
                score += gps_coord(*pos);
            }
        }
    }

    score
}

fn gps_coord(pos: (i32, i32)) -> i32 {
    pos.1 * 100 + pos.0
}

fn resize_map(map: Map) -> Map {
    let mut new_map = HashMap::new();

    for (pos, obj) in map.inner.iter() {
        let new_pos = (pos.0 * 2, pos.1);
        if let Object::Box(_) = obj {
            new_map.insert(new_pos, Object::Box(BoxPart::Left));
            new_map.insert((new_pos.0 + 1, new_pos.1), Object::Box(BoxPart::Right));
        } else if let Object::Wall = obj {
            new_map.insert(new_pos, obj.clone());
            new_map.insert((new_pos.0 + 1, new_pos.1), obj.clone());
        } else {
            new_map.insert(new_pos, obj.clone());
        }
    }

    Map { inner: new_map }
}

fn parse_map(contents: Vec<Vec<char>>) -> ((i32, i32), Map) {
    let mut map = HashMap::new();
    let mut robot_pos = (0, 0);
    for (y, row) in contents.iter().enumerate() {
        for (x, cell) in row.iter().enumerate() {
            let obj = match cell {
                '#' => Some(Object::Wall),
                '@' => Some(Object::Robot),
                'O' => Some(Object::Box(BoxPart::Single)),
                _ => None
            };

            if let Some(obj) = obj {
                if obj == Object::Robot {
                    robot_pos = (x as i32, y as i32);
                }

                map.insert((x as i32, y as i32), obj);
            }
        }
    }

    (robot_pos, Map { inner: map })
}

fn parse_moves(contents: Vec<char>) -> Vec<Direction> {
    contents.iter().map(|c| match c {
        '^' => Direction::Up,
        'v' => Direction::Down,
        '<' => Direction::Left,
        '>' => Direction::Right,
        _ => panic!("Invalid move")
    }).collect()
}

fn read_and_parse(file_path: String) -> ((i32, i32), Map, Vec<Direction>) {
    let contents = fs::read_to_string(file_path)
        .expect("Something went wrong reading the file");

    let mut map: Vec<Vec<char>> = vec![];
    let mut moves: Vec<char> = vec![];
    let mut in_map = true;
    for line in contents.lines() {
        if in_map {
            map.push(line.chars().collect());
        } else {
            moves.extend(line.chars());
        }
        
        if line == "" { in_map = false;  }
    }

    let (robot_pos, map) = parse_map(map);
    let moves = parse_moves(moves);

    (robot_pos, map, moves)
}