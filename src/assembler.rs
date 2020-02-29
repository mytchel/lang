use std::fmt;

use crate::lexer::Token;
use crate::parser::Expr;
use crate::parser::Stmt;

#[derive(Debug)]
pub enum Op {
	Print,
	Load,
	If,
	Goto,
	Add,
	Sub,
	Mul,
	Div,
}

#[derive(Debug)]
pub enum OpArg {
	Int(i64),
	Temp(usize),
}

#[derive(Debug)]
pub struct Ir {
	pub ret: Option<OpArg>,
	pub op: Op,
	pub arg1: OpArg,
	pub arg2: Option<OpArg>
}

impl fmt::Display for Op {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let s = match self {
			Op::Print  => "prt",
			Op::Load   => "ldr",
			Op::If     => "if ",
			Op::Goto   => "go ",
			Op::Add    => "add",
			Op::Sub    => "sub",
			Op::Mul    => "mul",
			Op::Div    => "div",
		};

		write!(f, "{}", s)
	}
}

impl fmt::Display for OpArg {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let s = match self {
			OpArg::Int(i) => format!("{}", i).to_string(),
			OpArg::Temp(i) => format!("t{}", i).to_string(),
		};

		write!(f, "{}", s)
	}
}

impl fmt::Display for Ir {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let mut s = if let Some(arg2) = &self.arg2 {
			format!("{} {} {}", self.op, self.arg1, arg2).to_string()
		} else {
			format!("{} {}", self.op, self.arg1).to_string()
		};

		if let Some(r) = &self.ret {
			s = format!("{} = {}", s, r).to_string();
		}
		
		write!(f, "{}", s)
	}
}

fn convert_op(t: &Token) -> Op {
	match t {
		Token::OpAdd => Op::Add,
		Token::OpSub => Op::Sub,
		Token::OpMul => Op::Mul,
		Token::OpDiv => Op::Div,
		_ => panic!("op {} not supported", t),
	}
}

fn convert_item(t: &Token) -> OpArg {
	match t {
		Token::Integer(i) => OpArg::Int(*i),
		_ => panic!("item {} not supported", t),
	}
}

fn assemble_expr(mut i: usize, e: &Box<Expr>) -> 
	(usize, Vec<Ir>)
{
	match &**e {
		Expr::Start(v, n) => {
			let (i, mut ir_a) = assemble_expr(i, &v);
			if let Some(next) = n {
				let (i, mut ir_b) = assemble_expr(i, &next);
				ir_a.append(&mut ir_b);
				(i, ir_a)
			} else {
				(i, ir_a)
			}
		},
		Expr::Op(o, v, n) => {
			let (value, mut ir_a) = assemble_expr(i, &v);
			let ret = value + 1;

			let ir_b = Ir { 
				ret: Some(OpArg::Temp(ret)),
				op: convert_op(o), 
				arg1: OpArg::Temp(i),
				arg2: Some(OpArg::Temp(value)),
			};

			ir_a.push(ir_b);
		
			i = ret;
			if let Some(next) = n {
				let (i, mut ir_c) = assemble_expr(i, &next);
				ir_a.append(&mut ir_c);
				(i, ir_a)
			} else {
				(i, ir_a)
			}
		},
		Expr::Item(item) => {
			i = i + 1;

			let ir = Ir { 
				ret: Some(OpArg::Temp(i)),
				op: Op::Load, 
				arg1: convert_item(item),
				arg2: None,
			};

			(i, vec![ir])
		},
		Expr::Call(_, _) =>
			panic!("cannot handle calls yet"),
	}
}

fn assemble_stmt_expr(e: Box<Expr>) -> Vec<Ir>
{
	let (t, mut ir) = assemble_expr(0, &e);

	let print_ir = Ir { 
		ret: None,
		op: Op::Print, 
		arg1: OpArg::Temp(t),
		arg2: None,
	};

	ir.push(print_ir);

	ir
}

fn assemble_stmt_if(cond: Box<Expr>, 
	then: Box<Stmt>, otherwise: Option<Box<Stmt>>) 
	-> Vec<Ir>
{
	let mut ir: Vec<Ir> = vec![];

	let (cond_t, mut cond_ir) = assemble_expr(0, &cond);

	let mut then_ir = assemble_stmt(*then);

	let mut other_ir = match otherwise {
		Some(o) => assemble_stmt(*o),
		None => vec![],
	};

	if other_ir.len() > 0 {
		let skip_ir = Ir {
			ret: None,
			op: Op::Goto,
			arg1: OpArg::Int((1 + other_ir.len()) as i64),
			arg2: None
		};

		then_ir.push(skip_ir);
	}
		
	/* if not true then jump forward by arg2 
		instructions */

	let if_ir = Ir { 
		ret: None,
		op: Op::If, 
		arg1: OpArg::Temp(cond_t),
		arg2: Some(OpArg::Int((1 + then_ir.len()) as i64)),
	};

	ir.append(&mut cond_ir);
	ir.push(if_ir);
	ir.append(&mut then_ir);
	ir.append(&mut other_ir);
	
	ir
}

fn assemble_stmt_list(l: Vec<Stmt>) -> Vec<Ir>
{
	let mut v: Vec<Ir> = vec![];

	for s in l {
		let mut vv = assemble_stmt(s);
		v.append(&mut vv);
	}

	v
}

fn assemble_stmt(s: Stmt) -> Vec<Ir>
{
	match s {
		Stmt::If(cond, then, otherwise) =>
			assemble_stmt_if(cond, then, otherwise),
		Stmt::Expr(e) =>
			assemble_stmt_expr(e),
		Stmt::List(l) => 
			assemble_stmt_list(l),
	}
}

pub fn assemble(s: Stmt) -> Vec<Ir> {
	assemble_stmt(s)
}

