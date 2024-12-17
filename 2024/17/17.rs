fn main() {
    let (_, registers) = execute(&vec![2, 6], (0, 0, 9));
    assert_eq!(1, registers.1);

    assert_eq!("0,1,2", part1((10, 0, 0), "5,0,5,1,5,4"));

    assert_eq!("4,2,5,6,7,7,7,7,3,1,0", part1((2024, 0, 0), "0,1,5,4,3,0"));
    
    let (_, registers) = execute(&vec![1, 7], (0, 29, 0));
    assert_eq!(26, registers.1);

    let (_, registers) = execute(&vec![4,0], (0, 2024, 43690));
    assert_eq!(44354, registers.1);

    assert_eq!("4,6,3,5,6,3,5,2,1,0", part1((729, 0, 0), "0,1,5,4,3,0"));

    println!("Part 1: {}", part1((56256477, 0, 0), "2,4,1,1,7,5,1,5,0,3,4,3,5,5,3,0"));

    assert_eq!(117440, part2("0,3,5,4,3,0"));
    
    println!("Part 2: {}", part2("2,4,1,1,7,5,1,5,0,3,4,3,5,5,3,0"));
}

fn part1(registers: (i64, i64, i64), program: &str) -> String {
    let program: Vec<i64> = program.split(",").map(|x| x.parse().unwrap()).collect();

    let (output, _) = execute(&program, registers);

    output.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(",")
}

fn part2(program: &str) -> i64 {
    let program: Vec<i64> = program.split(",").map(|x| x.parse().unwrap()).collect();
    let mut possible_as = vec![0];

    for i in 0..program.len() {
        let current_output = &program[program.len() - i - 1..];
        let mut new_possible_as = vec![];
        for a in &possible_as {
            for j in 0..8 {
                let next_a = 8 * a + j;
                
                let (output, _) = execute(&program, (next_a, 0, 0));
                if output == current_output {
                    new_possible_as.push(next_a);
                }
            }
        }

        possible_as = new_possible_as;
    }

    return *possible_as.iter().min().unwrap();
}

fn execute(program: &Vec<i64>, registers: (i64, i64, i64)) -> (Vec<i64>, (i64, i64, i64)) {
    let mut registers = registers;
    let mut output = vec![];
    let mut i = 0;

    while i < program.len() {
        let opcode = program[i];
        let (out, jump, new_registers) = get_instruction(opcode)(program[i + 1], registers);

        registers = new_registers;

        if let Some(out) = out {
            output.push(out);
        }

        if let Some(jump) = jump {
            i = jump(i);
        } else {
            i += 2;
        }
    }

    (output, registers)
}

fn get_combo_operand(operand: i64, registers: (i64, i64, i64)) -> i64 {
    match operand {
        0..4 => operand,
        4 => registers.0,
        5 => registers.1,
        6 => registers.2,
        _ => panic!("Invalid operand")
    }
}

fn adv(operand: i64, registers: (i64, i64, i64)) -> (Option<i64>, Option<Box<dyn Fn(usize) -> usize>>, (i64, i64, i64)) {
    let mut registers = registers.clone();
    registers.0 /= 2_i64.pow(get_combo_operand(operand, registers) as u32);

    (None, None, registers)
}

fn bxl(operand: i64, registers: (i64, i64, i64)) -> (Option<i64>, Option<Box<dyn Fn(usize) -> usize>>, (i64, i64, i64)) {
    let mut registers = registers.clone();
    registers.1 ^= operand;

    (None, None, registers)
}

fn bst(operand: i64, registers: (i64, i64, i64)) -> (Option<i64>, Option<Box<dyn Fn(usize) -> usize>>,(i64, i64, i64)) {
    let mut registers = registers.clone();
    registers.1 = get_combo_operand(operand, registers) % 8;

    (None, None, registers)
}

fn jnz(operand: i64, registers: (i64, i64, i64)) -> (Option<i64>, Option<Box<dyn Fn(usize) -> usize>>, (i64, i64, i64)) {
    let jump = if registers.0 != 0 {
        Some(Box::new(move |_: usize| operand as usize) as Box<dyn Fn(usize) -> usize>)
    } else {
        None
    };

    (None, jump, registers.clone())
}

fn bxc(_operand: i64, registers: (i64, i64, i64)) -> (Option<i64>, Option<Box<dyn Fn(usize) -> usize>>, (i64, i64, i64)) {
    let mut registers = registers.clone();
    registers.1 ^= registers.2;

    (None, None, registers)
}

fn out(operand: i64, registers: (i64, i64, i64)) -> (Option<i64>, Option<Box<dyn Fn(usize) -> usize>>, (i64, i64, i64)) {
    (Some(get_combo_operand(operand, registers) % 8), None, registers.clone())
}

fn bdv(operand: i64, registers: (i64, i64, i64)) -> (Option<i64>, Option<Box<dyn Fn(usize) -> usize>>, (i64, i64, i64)) {
    let mut registers = registers.clone();
    registers.1 = registers.0 / 2_i64.pow(get_combo_operand(operand, registers) as u32);

    (None, None, registers)
}

fn cdv(operand: i64, registers: (i64, i64, i64)) -> (Option<i64>, Option<Box<dyn Fn(usize) -> usize>>, (i64, i64, i64)) {
    let mut registers = registers.clone();
    registers.2 = registers.0 / 2_i64.pow(get_combo_operand(operand, registers) as u32);

    (None, None, registers)
}

fn get_instruction(opcode: i64) -> fn(i64, (i64, i64, i64)) -> (Option<i64>, Option<Box<dyn Fn(usize) -> usize>>, (i64, i64, i64)) {
    match opcode {
        0 => adv,
        1 => bxl,
        2 => bst,
        3 => jnz,
        4 => bxc,
        5 => out,
        6 => bdv,
        7 => cdv,
        _ => panic!("Invalid opcode")
    }
}

