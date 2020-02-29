use std::collections::HashMap;

use crate::assembler::Op;
use crate::assembler::OpArg;
use crate::assembler::Ir;

fn eval_op(temps: &mut HashMap<usize, i64>, o: &Ir) -> i64 
{
	match o.op {
		Op::Print => {
			match o.arg1 {
				OpArg::Temp(t) => {
					match temps.get(&t) {
						Some(i) => println!("t{} = {}", t, *i),
						None => panic!("temp {} not found", t),
					}
				},
				_ => panic!("print expected temp"),
			};

			1
		},
		
		Op::Load => {
			let v = match o.arg1 {
				OpArg::Int(i) => i,
				_ => panic!("load expected int value"),
			};

			match o.ret {
				Some(OpArg::Temp(t)) => temps.insert(t, v),
				_ => panic!("load expected temp"),
			};

			1
		},
		
		Op::If => {
			let cond = match o.arg1 {
				OpArg::Temp(t) => {
					match temps.get(&t) {
						Some(i) => *i,
						None => panic!("temp {} not found", t),
					}
				},
				_ => panic!("if expected cond temp"),
			};

			let skip = match o.arg2 {
				Some(OpArg::Int(i)) => i,
				_ => panic!("if expected int arg2"),
			};

			println!("if {} else skip {}", cond, skip);

			if cond != 0 {
				1
			} else {
				skip
			}
		},
		
		Op::Goto => {
			println!("goto {}", o.arg1);
			match o.arg1 {
				OpArg::Int(i) => i,
				_ => panic!("goto expected int"),
			}
		},
		
		Op::Add | Op::Sub | Op::Mul | Op::Div => {
			let a = match o.arg1 {
				OpArg::Int(i) => i,
				OpArg::Temp(t) => {
					match temps.get(&t) {
						Some(i) => *i,
						None => panic!("temp {} not found", t),
					}
				},
			};

			let b = match o.arg2 {
				Some(OpArg::Int(i)) => i,
				Some(OpArg::Temp(t)) => {
					match temps.get(&t) {
						Some(i) => *i,
						None => panic!("temp {} not found", t),
					}
				},
				_ => panic!("op expected arg2"),
			};

			let v = match o.op {
				Op::Add => a + b,
				Op::Sub => a - b,
				Op::Mul => a * b,
				Op::Div => a / b,
				_ => panic!("bad op somehow?"),
			};

			match o.ret {
				Some(OpArg::Temp(t)) => {
					temps.insert(t, v);
				},
				_ => panic!("load expected temp"),
			};

			1
		},
	}
}

pub fn eval(ops: Vec<Ir>) 
{
	let mut i: i64 = 0;

	let mut temps = HashMap::new();

	while (i as usize) < ops.len() {
		i += eval_op(&mut temps, &ops[i as usize]);
	}
}

