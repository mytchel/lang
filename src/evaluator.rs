use std::collections::HashMap;

use crate::assembler::Op;
use crate::assembler::OpArg;
use crate::assembler::Ir;

fn find_label(ops: &Vec<Ir>, label: &String) -> usize
{
	let mut i: usize = 0;

	while i < ops.len() {
		let o = &ops[i];
		match o.op {
			Op::Label => match &o.arg1 {
				OpArg::String(s) => {
					if s == label {
						return i;
					}
				},
				_ => panic!("label expected string arg"),
			},
			_ => (),
		};

		i = i + 1;
	}

	panic!("label {} not found!", label);
}

fn eval_op(temps: &mut HashMap<usize, i64>, i: usize, ops: &Vec<Ir>)
	-> usize
{
	let o = &ops[i];
	match o.op {
		Op::Label => {
			i + 1
		},	
			
		Op::Goto => {
			match &o.arg1 {
				OpArg::String(s) => find_label(ops, &s),
				_ => panic!("goto expected label"),
			}
		},

		Op::Print => {
			match o.arg1 {
				OpArg::Temp(t) => {
					match temps.get(&t) {
						Some(i) => println!("{}", *i),
						None => panic!("temp {} not found", t),
					}
				},
				_ => panic!("print expected temp"),
			};

			i + 1
		},
		
		Op::Load => {
			let v = match o.arg1 {
				OpArg::Int(i) => i,
				OpArg::Temp(t) => {
					match temps.get(&t) {
						Some(i) => *i,
						None => panic!("temp {} not found", t),
					}
				},
				_ => panic!("load expected int or temp"),
			};

			match o.ret {
				Some(OpArg::Temp(t)) => temps.insert(t, v),
				_ => panic!("load expected temp"),
			};

			i + 1
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

			let skip_index = match &o.arg2 {
				Some(OpArg::String(s)) => find_label(ops, &s),
				_ => panic!("if expected label arg2"),
			};

			if cond != 0 {
				i + 1
			} else {
				skip_index
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
				_ => panic!("math expected int or temp"),
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

			i + 1
		},
	}
}

pub fn eval(ops: Vec<Ir>) 
{
	let mut i: usize = 0;

	let mut temps = HashMap::new();

	while i < ops.len() {
		i = eval_op(&mut temps, i, &ops);
	}
}

