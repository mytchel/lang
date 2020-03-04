use std::fmt;

use crate::lexer::Token;
use crate::parser::Expr;
use crate::parser::Stmt;
use crate::parser::Fn;
use crate::parser::Prog;

#[derive(Debug)]
pub enum Op {
	Label,
	Goto,
	Print,
	Load,
	If,
	Add,
	Sub,
	Mul,
	Div,
}

#[derive(Debug)]
pub enum OpArg {
	Int(i64),
	Temp(usize),
	String(String),
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
	generated_labels: usize,
	outer: Option<&'a Env<'a>>,
}

impl fmt::Display for Op {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let s = match self {
			Op::Label  => "lbl",
			Op::Goto   => "go ",
			Op::Print  => "prt",
			Op::Load   => "ldr",
			Op::If     => "if ",
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
			OpArg::String(i) => format!("{}", i).to_string(),
		};

		write!(f, "{}", s)
	}
}

impl fmt::Display for Ir {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let mut s = format!("{} ", self.op).to_string();
		
		if let Some(r) = &self.ret {
			s = format!("{}{}, ", s, r).to_string();
		}
			
		s = format!("{}{}", s, self.arg1).to_string();
		
		if let Some(arg2) = &self.arg2 {
			s = format!("{}, {}", s, arg2).to_string()
		};
			
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

fn gen_label_name(env: &mut Env, n: String) -> String
{
	let i = env.generated_labels;
	env.generated_labels += 1;

	let mut s = "_".to_string();
	let mut e: &Env = env;
	while let Some(ee) = e.outer {
		s = format!("{}_", s).to_string();
		e = ee;
	}

	format!("{}{}_{}", s, n, i).to_string()
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

	/* Generate this here so the numbers increment how I'd 
	   expect them to. */
	let label_name = gen_label_name(env, "then_end".to_string());

	if other_ir.len() > 0 {
		let label_name = gen_label_name(env, "else_end".to_string());

		let other_end_label = Ir {
			ret: None,
			op: Op::Label,
			arg1: OpArg::String(label_name.to_string()),
			arg2: None
		};

		other_ir.push(other_end_label);

		let skip_other_ir = Ir {
			ret: None,
			op: Op::Goto,
			arg1: OpArg::String(label_name.to_string()),
			arg2: None
		};
		
		then_ir.push(skip_other_ir);
	}

	let then_end_label = Ir {
		ret: None,
		op: Op::Label,
		arg1: OpArg::String(label_name.to_string()),
		arg2: None
	};

	then_ir.push(then_end_label);

	/* if not true then jump to arg2 label */

	let if_ir = Ir { 
		ret: None,
		op: Op::If, 
		arg1: OpArg::Temp(cond_t),
		arg2: Some(OpArg::String(label_name.to_string())),
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
		generated_labels: 0,
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

fn assemble_fn(env: &mut Env, name: String, f: Fn) -> Vec<Ir>
{
	let mut n_env = Env { 
		vars: vec![], 
		generated_labels: 0,
		outer: Some(env),
	};

	let (t, mut ir) = assemble_stmt(&mut n_env, 0, f.stmts);

	let fn_label = Ir {
		ret: None,
		op: Op::Label,
		arg1: OpArg::String(name.to_string()),
		arg2: None
	};

	ir.insert(0, fn_label);

	ir
}

pub fn assemble(p: Prog) -> Vec<Ir> {
	let mut env = Env { 
		vars: vec![], 
		generated_labels: 0,
		outer: None 
	};

	let mut irs: Vec<Ir> = vec![];
	
	for (n, f) in p.funcs {
		let mut i = assemble_fn(&mut env, n, f);
		irs.append(&mut i);
	}

	let start = Ir {
		ret: None,
		op: Op::Goto,
		arg1: OpArg::String("main".to_string()),
		arg2: None
	};
	
	irs.insert(0, start);

	irs
}

