extern crate lazy_static;

use lazy_static::lazy_static;
use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
    static ref CACHE: Mutex<HashMap<(i32, i32), i64>> = Mutex::new(HashMap::new());
}

fn main() {
    assert_eq!(55_312, blink("125 17".to_string(), 25));

    println!("Part 1: {}", blink("5 89749 6061 43 867 1965860 0 206250".to_string(), 25));

    println!("Part 2: {}", blink("5 89749 6061 43 867 1965860 0 206250".to_string(), 75));
}

fn blink(input: String, blink_count: i32) -> i64 {
    let stones = input.split(" ").map(|x| x.parse::<i64>().unwrap()).collect::<Vec<i64>>();
    
    let mut stone_count = 0;
    for stone in stones {
        stone_count += get_count(stone, blink_count);
    }

    stone_count
}

fn get_count(stone: i64, blinks: i32) -> i64 {
    if blinks == 0 {
        return 1;
    }

    let cache = CACHE.lock().unwrap();
    if stone < 1000 && cache.contains_key(&(stone as i32, blinks)) {
        return cache[&(stone as i32, blinks)];
    }
    drop(cache);

    let count: i64;
    if stone == 0 {
        count = get_count(1, blinks - 1);
    } else {
        let stone_length = stone.ilog10() + 1;
        if stone_length % 2 == 0 {
            let ten_exp = 10_i64.pow(stone_length/2);
            count = get_count(stone / ten_exp, blinks - 1) + get_count(stone % ten_exp, blinks - 1);
        } else {
            count = get_count(stone * 2024, blinks - 1);
        }
    }

    if stone < 1000 {
        let mut cache = CACHE.lock().unwrap();
        cache.insert((stone as i32, blinks), count);
    }

    count
}