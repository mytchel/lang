use std::fmt;

use crate::lexer::Token;
use crate::parser::Type;
use crate::parser::Expr;
use crate::parser::Stmt;
use crate::parser::Fn;
use crate::parser::Prog;

#[derive(Debug)]
pub enum Op {
	Exit,
	Label,
	Goto,
	Print,
	Load,
	If,

	Push,
	Pop,
	Call,
	Ret,

	Op(Token),
}

#[derive(Debug)]
pub enum OpArg {
	Int(i64),
	Temp(usize),
	Label(String),
}

#[derive(Debug)]
pub struct Ir {
	pub ret: Option<OpArg>,
	pub op: Op,
	pub arg1: Option<OpArg>,
	pub arg2: Option<OpArg>
}

pub struct Env<'a> {
	vars: Vec<(String, Type, usize)>,
	generated_labels: usize,
	outer: Option<&'a Env<'a>>,
}

impl fmt::Display for Op {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let s = match self {
			Op::Exit   => "exit".to_string(),
			Op::Label  => "label".to_string(),
			Op::Goto   => "go".to_string(),
			Op::Print  => "print".to_string(),
			Op::Load   => "ldr".to_string(),
			Op::If     => "if".to_string(),
    		Op::Push   => "push".to_string(),
			Op::Pop    => "pop".to_string(),
			Op::Call   => "call".to_string(),
			Op::Ret    => "ret".to_string(),
	
	/*		
			Op::Add    => "add",
			Op::Sub    => "sub",
			Op::Mul    => "mul",
			Op::Div    => "div",
    		Op::Eq     => "eq",
			Op::Ne     => "ne",
			*/
			Op::Op(t)  => format!("op({})", t.to_string()),
		};

		write!(f, "{}", s)
	}
}

impl fmt::Display for OpArg {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let s = match self {
			OpArg::Int(i) => format!("{}", i).to_string(),
			OpArg::Temp(i) => format!("t{}", i).to_string(),
			OpArg::Label(i) => format!("{}", i).to_string(),
		};

		write!(f, "{}", s)
	}
}

impl fmt::Display for Ir {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let mut s = format!("{} ", self.op).to_string();
		
		if let Some(r) = &self.ret {
			s = format!("{}{} ", s, r).to_string();
		}

		if let Some(arg1) = &self.arg1 {
			s = format!("{} {} ", s, arg1).to_string();
		}
			
		if let Some(arg2) = &self.arg2 {
			s = format!("{} {}", s, arg2).to_string()
		};
			
		write!(f, "{}", s)
	}
}

fn convert_op(t: &Token) -> Op {
	match t {
		Token::OpAdd |
		Token::OpSub |
		Token::OpMul |
		Token::OpDiv |
		Token::OpRem |
		Token::CompEqual |
		Token::CompInEqual |
		Token::CompGreaterEqual |
		Token::CompGreater |
		Token::CompLessEqual |
		Token::CompLess |
		Token::CompAnd |
		Token::CompOr
		  => Op::Op(t.clone()),
		_ => panic!("op {} not supported", t),
	}
}

fn find_var(env: &Env, s: &String) -> Option<usize>
{
	for (v, _v_type, v_temp) in &env.vars {
		if v == s {
			return Some(*v_temp);
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

fn assemble_call(env: &mut Env, i: usize, 
	name: &String, args: &Vec<Expr>) -> (usize, Vec<Ir>)
{
	let mut irs: Vec<Ir> = vec![];

	let mut arg_i = i;
	for a in args {
		let (value, mut ir_e) = assemble_expr(env, arg_i, a);
		irs.append(&mut ir_e);

		arg_i = arg_i + 1;
		
		let ir = Ir { 
			ret: Some(OpArg::Temp(arg_i)),
			op: Op::Load, 
			arg1: Some(OpArg::Temp(value)),
			arg2: None,
		};

		irs.push(ir);
	}

	for j in 1..(i+1) {
		let ir = Ir { 
			ret: None,
			op: Op::Push, 
			arg1: Some(OpArg::Temp(j)),
			arg2: None,
		};

		irs.push(ir);
	}

	for j in 1..(arg_i - i + 1) {
		let ir = Ir { 
			ret: Some(OpArg::Temp(j)),
			op: Op::Load, 
			arg1: Some(OpArg::Temp(j + i)),
			arg2: None,
		};

		irs.push(ir);
	}

	let ir = Ir { 
		ret: None,
		op: Op::Call, 
		arg1: Some(OpArg::Label(name.to_string())),
		arg2: None,
	};

	irs.push(ir);

	let ret = i + 1;

	let ir = Ir { 
		ret: Some(OpArg::Temp(ret)),
		op: Op::Load, 
		arg1: Some(OpArg::Temp(1)),
		arg2: None,
	};
	
	irs.push(ir);

	for j in (1..(i+1)).rev() {
		let ir = Ir { 
			ret: Some(OpArg::Temp(j)),
			op: Op::Pop, 
			arg1: None,
			arg2: None,
		};

		irs.push(ir);
	}

	(ret, irs)
}

fn assemble_expr(env: &mut Env, mut i: usize, e: &Expr) -> 
	(usize, Vec<Ir>)
{
	match e {
    	Expr::TempStart(_, _) | Expr::TempOp(_, _, _) => {
    	   panic!("Shouldn't get unfixed expressions")
		},
        
        Expr::Op(token, a, b) => {
            let mut ir = vec![];
            
            let (value_a, mut ir_a) = assemble_expr(env, i, &a);
            ir.append(&mut ir_a);
            i = value_a;
            
            let (value_b, mut ir_b) = assemble_expr(env, i, &b);
            ir.append(&mut ir_b);
            i = value_b;

            let ret = i + 1;
    
   			let ir_op = Ir { 
   				ret: Some(OpArg::Temp(ret)),
   				op: convert_op(token), 
   				arg1: Some(OpArg::Temp(value_a)),
   				arg2: Some(OpArg::Temp(value_b)),
   			};
    
   			ir.push(ir_op);
    		
   			(ret, ir)
        },

		Expr::Item(item) => {
			i = i + 1;

			let ir = Ir { 
				ret: Some(OpArg::Temp(i)),
				op: Op::Load, 
				arg1: Some(convert_item(env, item)),
				arg2: None,
			};

			(i, vec![ir])
		},

		Expr::Call(name, args) => 
			assemble_call(env, i, name, args),
	}
}

fn assemble_stmt_expr(env: &mut Env, t: usize, 
	expr: Box<Expr>) 
	-> (usize, Vec<Ir>)
{
	let (_ret_t, ir) = assemble_expr(env, t, &expr);

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
			arg1: Some(OpArg::Label(label_name.to_string())),
			arg2: None
		};

		other_ir.push(other_end_label);

		let skip_other_ir = Ir {
			ret: None,
			op: Op::Goto,
			arg1: Some(OpArg::Label(label_name.to_string())),
			arg2: None
		};
		
		then_ir.push(skip_other_ir);
	}

	let then_end_label = Ir {
		ret: None,
		op: Op::Label,
		arg1: Some(OpArg::Label(label_name.to_string())),
		arg2: None
	};

	then_ir.push(then_end_label);

	/* if not true then jump to arg2 label */

	let if_ir = Ir { 
		ret: None,
		op: Op::If, 
		arg1: Some(OpArg::Temp(cond_t)),
		arg2: Some(OpArg::Label(label_name.to_string())),
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
	var_name: String, var_type: Type, value: Box<Expr>)
	-> (usize, Vec<Ir>)
{
	let (value_t, mut value_ir) = assemble_expr(env, t, &value);

	t = t + 1;

	let ir = Ir { 
		ret: Some(OpArg::Temp(t)),
		op: Op::Load, 
		arg1: Some(OpArg::Temp(value_t)),
		arg2: None,
	};

	value_ir.push(ir);

	env.vars.push((var_name.to_string(), var_type, t));

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
		arg1: Some(OpArg::Temp(value_t)),
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

fn assemble_stmt_return(env: &mut Env, t: usize,
	expr: Option<Box<Expr>>)
	-> (usize, Vec<Ir>)
{
	let (value_t, mut ir) = match expr {
		Some(e) => assemble_expr(env, t, &e),
		None => (t, vec![]),
	};

	let ldr_ret = Ir {
		ret: Some(OpArg::Temp(1)),
		op: Op::Load,
		arg1: Some(OpArg::Temp(value_t)),
		arg2: None
	};

	ir.push(ldr_ret);

	let ret = Ir {
		ret: None,
		op: Op::Ret,
		arg1: None,
		arg2: None
	};

	ir.push(ret);

	(t, ir)
}

fn assemble_fn(env: &mut Env, name: String, f: Fn) -> Vec<Ir>
{
	let mut n_env = Env { 
		vars: vec![], 
		generated_labels: 0,
		outer: Some(env),
	};

	let mut p_t = 0;
	for (param_name, param_type) in f.params {
		p_t += 1;
		n_env.vars.push((param_name, param_type, p_t));
	}

	let (_t, mut ir) = assemble_stmt(&mut n_env, p_t, f.stmts);

	let fn_label = Ir {
		ret: None,
		op: Op::Label,
		arg1: Some(OpArg::Label(name.to_string())),
		arg2: None
	};

	ir.insert(0, fn_label);

	let ret = Ir {
		ret: None,
		op: Op::Ret,
		arg1: None,
		arg2: None
	};

	ir.push(ret);

	ir
}

fn assemble_stmt(env: &mut Env, t: usize, s: Stmt) -> (usize, Vec<Ir>)
{
	match s {
		Stmt::Return(expr) =>
			assemble_stmt_return(env, t, expr),
		Stmt::Alloc(var_name, var_type, value) =>
			assemble_stmt_alloc(env, t, var_name, var_type, value),
		Stmt::Assign(name, value) =>
			assemble_stmt_assign(env, t, name, value),
		Stmt::If(cond, then, otherwise) =>
			assemble_stmt_if(env, t, cond, then, otherwise),
		Stmt::List(l) => 
			assemble_stmt_list(env, t, l),
		Stmt::Expr(e) =>
			assemble_stmt_expr(env, t, e),
	}
}

pub fn assemble(p: Prog) -> Vec<Ir> {
	let mut env = Env { 
		vars: vec![], 
		generated_labels: 0,
		outer: None 
	};

	let mut irs: Vec<Ir> = vec![];

	let start = Ir {
		ret: None,
		op: Op::Call,
		arg1: Some(OpArg::Label("main".to_string())),
		arg2: None
	};
	
	irs.push(start);

	let exit = Ir {
		ret: None,
		op: Op::Exit,
		arg1: None,
		arg2: None
	};
	
	irs.push(exit);
	
	for (n, f) in p.funcs {
		let mut i = assemble_fn(&mut env, n, f);
		irs.append(&mut i);
	}

	irs
}

