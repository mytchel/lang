use std::collections::HashMap;

use crate::lexer::Token;
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
				Some(OpArg::Label(s)) => {
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

fn pretty_print_op(op: &Ir,
	temps: &mut HashMap<usize, i64>,
	_stack: &mut Vec<i64>)
{
	print!("{}", op.op);

	if let Some(r) = &op.ret {
		print!(" ");
		print!("={}", r);
	}

	if let Some(a) = &op.arg1 {
		print!(" ");
		match a {
			OpArg::Int(i) => print!("{}", i),
			OpArg::Temp(t) => {
				print!("t{}=", t);
				match temps.get(&t) {
					Some(v) => print!("{}", *v),
					None => panic!("um"),
				}
			},
			OpArg::Label(s) => print!("{}", s),
		}
	}

	if let Some(a) = &op.arg2 {
		print!(" ");
		match a {
			OpArg::Int(i) => print!("{}", i),
			OpArg::Temp(t) => {
				print!("t{}=", t);
				match temps.get(&t) {
					Some(v) => print!("{}", *v),
					None => panic!("um"),
				}
			},
			OpArg::Label(s) => print!("'{}'", s),
		}
	}

	println!("");
	std::thread::sleep(std::time::Duration::from_millis(100));
}

fn eval_op(ops: &Vec<Ir>,
	i: usize,
	temps: &mut HashMap<usize, i64>,
	stack: &mut Vec<i64>)
	-> usize
{
	let o = &ops[i];

//	pretty_print_op(o, temps, stack);

	match &o.op {
		Op::Exit => {
			std::process::exit(0)
		},
		Op::Label => {
			i + 1
		},	
			
		Op::Goto => {
			match &o.arg1 {
				Some(OpArg::Label(s)) => find_label(ops, &s),
				_ => panic!("goto expected label"),
			}
		},

		Op::Call => {
			match &o.arg1 {
				Some(OpArg::Label(s)) => {
					match s.as_str() {
						"print" => {
							match temps.get(&1) {
								Some(v) => {
									println!("{}", v);
									i + 1
								},
								None => panic!("print expected t1"),
							}
						},
						_ => {
							stack.push(i as i64 + 1);
							find_label(ops, &s)
						},
					}
				},
				_ => panic!("call expected label"),
			}
		},

		Op::Ret => {
			match stack.pop() {
				Some(n) => n as usize,
				None => panic!("ret with empty stack"),
			}
		},

		Op::Push => {
			match o.arg1 {
				Some(OpArg::Temp(t)) => {
					match temps.get(&t) {
						Some(i) => stack.push(*i),
						None => panic!("temp {} not found", t),
					}
				},
				_ => panic!("print expected temp"),
			};

				i + 1
		},

		Op::Pop => {
			let v = match stack.pop() {
				Some(i) => i,
				_ => panic!("pop with empty stack"),
			};

			match o.ret {
				Some(OpArg::Temp(t)) => temps.insert(t, v),
				_ => panic!("pop expected temp"),
			};

			i + 1
		},

		Op::Print => {
			match &o.arg1 {
				Some(OpArg::Temp(t)) => {
					match temps.get(&t) {
						Some(i) => println!("{}", *i),
						None => panic!("temp {} not found", t),
					}
				},
				_ => panic!("print expected temp/var"),
			};

			i + 1
		},
		
		Op::Load => {
			let v = match &o.arg1 {
				Some(OpArg::Int(i)) => *i,
				Some(OpArg::Temp(t)) => {
					match temps.get(&t) {
						Some(i) => *i,
						None => panic!("temp {} not found", t),
					}
				},
				_ => panic!("load expected int, temp, or var"),
			};

			match &o.ret {
				Some(OpArg::Temp(t)) => temps.insert(*t, v),
				_ => panic!("load expected temp"),
			};

			i + 1
		},
		
		Op::If => {
			let cond = match o.arg1 {
				Some(OpArg::Temp(t)) => {
					match temps.get(&t) {
						Some(i) => *i,
						None => panic!("temp {} not found", t),
					}
				},
				_ => panic!("if expected cond temp"),
			};

			let skip_index = match &o.arg2 {
				Some(OpArg::Label(s)) => find_label(ops, &s),
				_ => panic!("if expected label arg2"),
			};

			if cond != 0 {
				i + 1
			} else {
				skip_index
			}
		},
	
		Op::Op(token) => {
			let a = match &o.arg1 {
				Some(OpArg::Int(i)) => *i,
				Some(OpArg::Temp(t)) => {
					match temps.get(t) {
						Some(i) => *i,
						None => panic!("temp {} not found", t),
					}
				},
				_ => panic!("math expected int or temp"),
			};

			let b = match &o.arg2 {
				Some(OpArg::Int(i)) => *i,
				Some(OpArg::Temp(t)) => {
					match temps.get(t) {
						Some(i) => *i,
						None => panic!("temp {} not found", t),
					}
				},
				_ => panic!("op expected arg2"),
			};

			let v = match token {
				Token::OpAdd => a + b,
				Token::OpSub => a - b,
				Token::OpMul => a * b,
				Token::OpDiv => a / b,
				Token::OpRem => a % b,
				t => {
				    let b = match t {
				        Token::CompEqual => a == b,
				        Token::CompInEqual => a != b,
				        Token::CompGreaterEqual => a >= b,
				        Token::CompGreater => a > b,
				        Token::CompLessEqual => a <= b,
				        Token::CompLess => a < b,
				        Token::CompAnd => a == 1 && b == 1,
				        Token::CompOr => a == 1 || b == 1,
				        _ => panic!("op {} unsupported", t),
				    };

				    if b {
				        1
				    } else {
				        0
				    }
                }
			};

			match &o.ret {
				Some(OpArg::Temp(t)) => temps.insert(*t, v),
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
	let mut stack: Vec<i64> = vec![];

	while i < ops.len() {
		i = eval_op(&ops, i, &mut temps, &mut stack);
	}
}

