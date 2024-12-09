use std::fs;

fn main() {
    assert_eq!(1928, part1("sample.txt".to_string()));

    println!("Part 1: {}", part1("input.txt".to_string()));

    assert_eq!(2858, part2("sample.txt".to_string()));

    println!("Part 2: {}", part2("input.txt".to_string()));
}

fn part1(file_path: String) -> i64 {
    let mut disk = read_and_parse(file_path);

    let mut j = disk.len() - 1;
    for i in 0..disk.len() {
        if let Some(_) = disk[i] { continue }

        while let None = disk[j] {
            j -= 1;
        }

        if i >= j { break; }

        disk.swap(i, j);
    }

    checksum(&disk)
}

fn part2(file_path: String) -> i64 {
    let mut disk = read_and_parse(file_path);

    let mut max_id = 0;
    for i in (0..disk.len()).rev() {
        if let Some(id) = disk[i] {
            max_id = max_id.max(id);
            break;
        }
    }
    
    for file_id in (0..max_id+1).rev() {
        let (start, end) = find(&disk, file_id);
        let length = end - start;

        for i in 0..start {
            if disk[i] != None { continue; }

            let fits = disk[i..i+length].iter().all(|x| x == &None);

            if fits {
                for j in 0..length {
                    disk.swap(i + j, start + j);
                }

                break;
            }
        }
    }

    checksum(&disk)
}

fn checksum(disk: &Vec<Option<i32>>) -> i64 {
    let mut sum = 0;

    for i in 0..disk.len() {
        if let Some(n) = disk[i] {
            sum += i as i64 * n as i64;
        }
    }

    sum
}

fn find(disk: &Vec<Option<i32>>, file_id: i32) -> (usize, usize) {
    let mut start = None;
    let mut end = None;
    for i in 0..disk.len() {
        if let Some(id) = disk[i] {
            if id == file_id && start == None {
                start = Some(i);
            } else if id != file_id && start != None {
                end = Some(i);
                break;
            }
        } else if start != None {
            end = Some(i);
            break;
        }
    }

    if end == None {
        end = Some(disk.len());
    }

    (start.unwrap(), end.unwrap())
}

fn parse(line: &str) -> Vec<Option<i32>> {
    let mut disk = Vec::new();
    
    for (i, c) in line.chars().enumerate() {
        let val = c.to_digit(10).unwrap() as i32;

        for _ in 0..val {
            if i % 2 == 0 { disk.push(Some((i / 2) as i32)); }
            else { disk.push(None); }
        }
    }
    
    disk
}

fn read_and_parse(file_path: String) -> Vec<Option<i32>> {
    let contents = fs::read_to_string(file_path)
        .expect("Something went wrong reading the file");

    parse(contents.lines().next().unwrap())
}