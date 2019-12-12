fn main() {
	let file = std::fs::read_to_string("02-input.txt").unwrap();
	let p = file.trim().split(',').map(str::parse).map(Result::unwrap).collect();
	find_inputs(p);
}

fn get_op(opcode: i32) -> fn(i32, i32) -> i32 {
	use std::ops::{ Add, Mul };
	match opcode {
		1 => i32::add,
		2 => i32::mul,
		_ => unreachable!(),
	}
}

fn run_program(mut p: Vec<i32>, noun: i32, verb: i32) -> i32 {
	p[1] = noun;
	p[2] = verb;
	let mut i = 0;
	while p[i] != 99 {
		let opcode = p[i];
		let arg1 = p[p[i+1] as usize];
		let arg2 = p[p[i+2] as usize];
		let value = get_op(opcode)(arg1, arg2);
		let dst = p[i+3] as usize;
		p[dst] = value;
		i += 4;
	}
	p[0]
}

fn find_inputs(p: Vec<i32>) {
	for noun in 0..=99 {
		for verb in 0..=99 {
			if run_program(p.clone(), noun, verb) == 19690720 {
				println!("noun = {}", noun);
				println!("verb = {}", verb);
				println!("100 * noun + verb = {}", 100 * noun + verb);
				break;
			}
		}
	}
}