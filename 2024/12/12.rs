use std::fs;

fn main() {
    assert_eq!(140, part1("sample1.txt".to_string()));
    
    assert_eq!(772, part1("sample2.txt".to_string()));
    
    assert_eq!(1930, part1("sample3.txt".to_string()));

    println!("Part 1: {}", part1("input.txt".to_string()));

    assert_eq!(80, part2("sample1.txt".to_string()));

    assert_eq!(436, part2("sample2.txt".to_string()));

    assert_eq!(236, part2("sample4.txt".to_string()));

    assert_eq!(368, part2("sample5.txt".to_string()));

    assert_eq!(1206, part2("sample3.txt".to_string()));

    println!("Part 2: {}", part2("input.txt".to_string()));
}

#[derive(Debug,Clone)]
struct Side {
    positions: Vec<(usize, usize)>,
    dir: (i32, i32)
}

fn part1(file: String) -> i32 {
    let (map, regions) = read_and_parse(file);

    let mut sum = 0;
    for region in regions {
        sum += calc_perimiter(&region, &map) * region.len() as i32
    }

    sum
}

fn part2(file: String) -> i32 {
    let (map, regions) = read_and_parse(file);

    let mut sum = 0;
    for region in regions {
        sum += calc_sides(&region, &map) * region.len() as i32
    }

    sum
}

fn same_region(pos1: (usize, usize), pos2: (i32, i32), map: &Vec<Vec<usize>>) -> bool {
    pos2.0 >= 0 && pos2.0 < map[0].len() as i32
        && pos2.1 >= 0 && pos2.1 < map.len() as i32
        && map[pos2.1 as usize][pos2.0 as usize] == map[pos1.1][pos1.0]
}

fn calc_plant_sides(plant: &(usize, usize), map: &Vec<Vec<usize>>) -> i32 {
    let &(x, y) = plant;
    let mut perimiter = 0;

    let dirs = vec![(0, -1), (1, 0), (0, 1), (-1, 0)];
    for dir in dirs {
        if !same_region((x, y), (x as i32 + dir.0, y as i32 + dir.1), &map) {
            perimiter += 1;
        }
    }

    perimiter
}

fn rotate(dir: (i32, i32)) -> (i32, i32) {
    match dir {
        (-1, 0) => (0, 1),
        (0, 1) => (1, 0),
        (1, 0) => (0, -1),
        (0, -1) => (-1, 0),
        _ => panic!("Invalid dir!")
    }
}

fn find_contiguous_sides(side: Side, map: &Vec<Vec<usize>>) -> Vec<Side> {
    let mut sides = vec![side];
    loop {
        let side = sides.last_mut().unwrap();
        let pos = *side.positions.last().unwrap();
        let orth = rotate(side.dir);

        // The next position along the side
        let neighbor = (pos.0 as i32 + orth.0, pos.1 as i32 + orth.1);
        // The position *across the side* from the neighbor
        let neighbor_neighbor = (neighbor.0 + side.dir.0, neighbor.1 + side.dir.1);

        if same_region(pos, neighbor, &map) && !same_region(pos, neighbor_neighbor, &map) {
            side.positions.push((neighbor.0 as usize, neighbor.1 as usize));
        } else if !same_region(pos, neighbor, &map) {
            sides.push(Side {
                positions: vec![pos],
                dir: orth
            });
        } else {
            sides.push(Side {
                positions: vec![(neighbor_neighbor.0 as usize, neighbor_neighbor.1 as usize)],
                dir: (-orth.0, -orth.1)
            });
        }

        // We've looped around to the starting side
        if sides.len() > 4
            && sides[0].dir == sides.last().unwrap().dir
            && sides[0].positions[0] == *sides.last().unwrap().positions.last().unwrap() {

            let last_side = sides.pop().unwrap();

            let new_positions = &last_side.positions[..last_side.positions.len() - 1];
            sides[0].positions.extend(&*new_positions);

            return sides;
        }
    }
}

fn calc_sides(region: &Vec<(usize, usize)>, map: &Vec<Vec<usize>>) -> i32 {
    let perimiter = calc_perimiter(region, &map);

    let mut sides = Vec::<Side>::new();
    let mut sides_perimiter = 0;
    while sides_perimiter < perimiter {
        let mut found_side: Option<Side> = None;
        let dirs = vec![(0, -1), (1, 0), (0, 1), (-1, 0)];
        for pos in region {
            for dir in &dirs {
                // We've already found a side containing this edge - skip it
                if sides.iter().any(|s| s.dir == *dir && s.positions.contains(pos)) {
                    continue;
                }
    
                let neighbor = (pos.0 as i32 + dir.0, pos.1 as i32 + dir.1);
                if !same_region(*pos, neighbor, &map) {
                    found_side = Some(Side {
                        positions: vec![*pos],
                        dir: *dir
                    });
                    continue;
                }
            }
        }

        let contiguous_sides = find_contiguous_sides(found_side.unwrap(), &map);
        sides_perimiter += (*contiguous_sides).into_iter().map(|s| s.positions.len() as i32).sum::<i32>();
        sides.extend(contiguous_sides);
    }

    sides.len() as i32
}

fn calc_perimiter(region: &Vec<(usize, usize)>, map: &Vec<Vec<usize>>) -> i32 {
    let mut perimiter = 0;
    for &(x, y) in region {
        perimiter += calc_plant_sides(&(x, y), &map);
    }

    perimiter
}

fn find_region(pos: (usize, usize), map: &mut Vec<Vec<(char, Option<usize>)>>, region_num: usize) -> Vec<(usize, usize)> {
    let mut region = vec![pos.clone()];
    let mut new_plants = vec![pos.clone()];
    map[pos.1][pos.0].1 = Some(region_num);
    let type_ = map[pos.1][pos.0].0;

    while !new_plants.is_empty() {
        let mut next_plants = vec![];
        for plant in new_plants {
            let (x, y) = plant;
            if x > 0 && map[y][x - 1].1 == None && map[y][x - 1].0 == type_ {
                next_plants.push((x - 1, y));
                map[y][x - 1].1 = Some(region_num);
            }
            if x < map[0].len() - 1 && map[y][x + 1].1 == None  && map[y][x + 1].0 == type_ {
                next_plants.push((x + 1, y));
                map[y][x + 1].1 = Some(region_num);
            }
            if y > 0 && map[y - 1][x].1 == None  && map[y - 1][x].0 == type_ {
                next_plants.push((x, y - 1));
                map[y - 1][x].1 = Some(region_num);
            }
            if y < map.len() - 1 && map[y + 1][x].1 == None  && map[y + 1][x].0 == type_ {
                next_plants.push((x, y + 1));
                map[y + 1][x].1 = Some(region_num);
            }
        }

        new_plants = next_plants;
        region.extend(new_plants.clone());
    }

    region
}

fn read_and_parse(file_path: String) -> (Vec<Vec<usize>>, Vec<Vec<(usize, usize)>>) {
    let mut map = read(file_path)
        .into_iter()
        .map(|line| line.iter().map(|c| (*c, Option::<usize>::None)).collect())
        .collect::<Vec<Vec<(char, Option<usize>)>>>();

    let mut regions = vec![];
    let mut region_num = 0;
    for y in 0..map.len() {
        for x in 0..map[0].len() {
            if map[y][x].1.is_some() {
                continue;
            }

            region_num += 1;
            regions.push(find_region((x, y), &mut map, region_num));
        }
    }

    let region_map = map.into_iter()
        .map(|row| row.into_iter().map(|(_, region)| region.unwrap()).collect())
        .collect::<Vec<Vec<usize>>>();

    (region_map, regions)
}

fn read(file: String) -> Vec<Vec<char>> {
    let contents = fs::read_to_string(file)
        .expect("Something went wrong reading the file");

    contents.lines().map(|line| line.chars().collect()).collect()
}