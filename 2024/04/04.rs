use std::fs;

fn main() {
    assert_eq!(18, part1("sample.txt".to_string()));

    println!("Part 1: {}", part1("input.txt".to_string()));

    assert_eq!(9, part2("sample.txt".to_string()));

    println!("Part 2: {}", part2("input.txt".to_string()));
}


fn part1(file_path: String) -> i32 {
    let word_search = read_file(file_path);

    let mut words = Vec::<Word>::new();
    let directions = vec![
        (0, 1), (0, -1), (1, 0), (-1, 0),
        (1, 1), (-1, 1), (1, -1), (-1, -1)];
    for (y, line) in word_search.iter().enumerate() {
        for (x, c) in line.iter().enumerate() {
            if *c == 'X' {
                for (dx, dy) in &directions {
                    words.push(Word {
                        pos: Coord { x: x as i32, y: y as i32},
                        dir: Coord { x: *dx, y: *dy }
                    });
                }
            }
        }
    }

    for next_char in &vec!['M', 'A', 'S']
    {
        let prev_words = words;
        words = Vec::<Word>::new();
        for word in prev_words {
            let Some(next) = get_char(&word_search, Coord::add(&word.pos, &word.dir)) else { continue; };
 
            if *next == *next_char {
                let mut new_word = word.clone();
                new_word.pos = Coord::add(&word.pos, &word.dir);
                words.push(new_word);
            }
        }
    }

    words.len() as i32
}

fn part2(file_path: String) -> i32 {
    let word_search = read_file(file_path);

    let mut count = 0;
    for (y, line) in word_search.iter().enumerate() {
        for (x, c) in line.iter().enumerate() {
            if *c != 'A' { continue; }
            
            let Some(up_left) = get_char(&word_search, Coord { x: x as i32 - 1, y: y as i32 - 1 }) else { continue; };
            let Some(up_right) = get_char(&word_search, Coord { x: x as i32 + 1, y: y as i32 - 1 }) else { continue; };
            let Some(down_left) = get_char(&word_search, Coord { x: x as i32 - 1, y: y as i32 + 1 }) else { continue; };
            let Some(down_right) = get_char(&word_search, Coord { x: x as i32 + 1, y: y as i32 + 1 }) else { continue; };

            let diag_1 = (*up_left == 'M' && *down_right == 'S') || (*up_left == 'S' && *down_right == 'M');
            let diag_2 = (*up_right == 'M' && *down_left == 'S') || (*up_right == 'S' && *down_left == 'M');
            if diag_1 && diag_2 {
                count += 1;
            }
        }
    }

    count
}

fn get_char(word_search: &Vec<Vec<char>>, coord: Coord) -> Option<&char> {
    word_search.get(coord.y as usize)
        .and_then(|row| row.get(coord.x as usize))
}

fn read_file(file_path: String) -> Vec<Vec<char>> {
    fs::read_to_string(file_path)
        .expect("Something went wrong reading the file")
        .lines()
        .map(|line| line.chars().collect())
        .collect()
}

#[derive(Debug, Clone)]
struct Coord {
    x: i32,
    y: i32
}

impl Coord {
    fn add(&self, other: &Coord) -> Coord {
        Coord { x: self.x + other.x, y: self.y + other.y }
    }
}

#[derive(Debug, Clone)]
struct Word {
    pos: Coord,
    dir: Coord
}