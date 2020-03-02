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

pub struct Env<'a> {
	vars: Vec<(String, usize)>,
	outer: Option<&'a Env<'a>>,
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

fn find_var(env: &Env, s: &String) -> Option<usize>
{
	for (v, t) in &env.vars {
		if v == s {
			return Some(*t);
		}
	}

	match env.outer {
		Some(e) => find_var(e, s),
		None => None,
	}
}

fn convert_item(env: &Env, t: &Token) -> OpArg {
	match t {
		Token::Integer(i) => OpArg::Int(*i),
		Token::Symbol(s) => {
			match find_var(env, s) {
				Some(t) => OpArg::Temp(t),
				None => panic!("var {} not found", s),
			}
		},
		_ => panic!("item {} not supported", t),
	}
}

fn assemble_expr(env: &mut Env, mut i: usize, e: &Box<Expr>) -> 
	(usize, Vec<Ir>)
{
	match &**e {
		Expr::Start(v, n) => {
			let (i, mut ir_a) = assemble_expr(env, i, &v);
			if let Some(next) = n {
				let (i, mut ir_b) = assemble_expr(env, i, &next);
				ir_a.append(&mut ir_b);
				(i, ir_a)
			} else {
				(i, ir_a)
			}
		},
		Expr::Op(o, v, n) => {
			let (value, mut ir_a) = assemble_expr(env, i, &v);
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
				let (i, mut ir_c) = assemble_expr(env, i, &next);
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
				arg1: convert_item(env, item),
				arg2: None,
			};

			(i, vec![ir])
		},
		Expr::Call(_, _) =>
			panic!("cannot handle calls yet"),
	}
}

fn assemble_stmt_expr(env: &mut Env, t: usize, 
	e: Box<Expr>) 
	-> (usize, Vec<Ir>)
{
	let (t_ret, mut ir) = assemble_expr(env, t, &e);

	let print_ir = Ir { 
		ret: None,
		op: Op::Print, 
		arg1: OpArg::Temp(t_ret),
		arg2: None,
	};

	ir.push(print_ir);

	(t, ir)
}

fn assemble_stmt_if(env: &mut Env, t: usize, 
	cond: Box<Expr>, 
	then: Box<Stmt>, 
	otherwise: Option<Box<Stmt>>) 
	-> (usize, Vec<Ir>)
{
	let mut ir: Vec<Ir> = vec![];

	let (cond_t, mut cond_ir) = assemble_expr(env, t, &cond);

	let (_then_t, mut then_ir) = assemble_stmt(env, t, *then);

	let (_other_t, mut other_ir) = match otherwise {
		Some(o) => assemble_stmt(env, t, *o),
		None => (t, vec![]),
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

	/* TODO: should return then_t or other_t or
		a new t that is the same for both */

	(t, ir)
}

fn assemble_stmt_alloc(env: &mut Env, mut t: usize,
	name: String, value: Box<Expr>)
	-> (usize, Vec<Ir>)
{
	let (value_t, mut value_ir) = assemble_expr(env, t, &value);

	t = t + 1;

	let ir = Ir { 
		ret: Some(OpArg::Temp(t)),
		op: Op::Load, 
		arg1: OpArg::Temp(value_t),
		arg2: None,
	};

	println!("alloc {} as temp {}", name, t);

	value_ir.push(ir);

	env.vars.push((name, t));

	(t, value_ir)
}

fn assemble_stmt_assign(env: &mut Env, t: usize,
	name: String, value: Box<Expr>)
	-> (usize, Vec<Ir>)
{
	let v_temp = match find_var(env, &name) {
		Some(t) => t,
		None => panic!("assign to unknown var {}", name),
	};

	let (value_t, mut value_ir) = assemble_expr(env, t, &value);

	let ir = Ir { 
		ret: Some(OpArg::Temp(v_temp)),
		op: Op::Load, 
		arg1: OpArg::Temp(value_t),
		arg2: None,
	};

	value_ir.push(ir);

	(t, value_ir)
}

fn assemble_stmt_list(env: &mut Env, t: usize, l: Vec<Stmt>) 
	-> (usize, Vec<Ir>)
{
	let mut v: Vec<Ir> = vec![];

	let mut n_env = Env { 
		vars: vec![], 
		outer: Some(env),
	};

	let mut n_t = t;
	for s in l {
		let (tt, mut vv) = assemble_stmt(&mut n_env, n_t, s);
		v.append(&mut vv);
		n_t = tt;
	}

	(t, v)
}

fn assemble_stmt(env: &mut Env, t: usize, s: Stmt) -> (usize, Vec<Ir>)
{
	match s {
		Stmt::Alloc(name, value) =>
			assemble_stmt_alloc(env, t, name, value),
		Stmt::Assign(name, value) =>
			assemble_stmt_assign(env, t, name, value),
		Stmt::If(cond, then, otherwise) =>
			assemble_stmt_if(env, t, cond, then, otherwise),
		Stmt::Expr(e) =>
			assemble_stmt_expr(env, t, e),
		Stmt::List(l) => 
			assemble_stmt_list(env, t, l),
	}
}

pub fn assemble(s: Stmt) -> Vec<Ir> {
	let mut env = Env { 
		vars: vec![], 
		outer: None 
	};

	let (_t, irs) = assemble_stmt(&mut env, 0, s);
	irs
}

