use crate::lexer::Token;
use crate::parser::Expr;
use crate::parser::Stmt;

pub struct Op {
	op: Token,
	arg1: Token,
	arg2: Option<Token>
}

fn assemble_expr(mut i: usize, e: &Box<Expr>) -> usize
{
	match &**e {
		Expr::Start(v, n) => {
			i = assemble_expr(i, &v);
			if let Some(next) = n {
				i = assemble_expr(i, &next);
			}
		},
		Expr::Op(o, v, n) => {
			let value = assemble_expr(i, &v);
			let ret = value + 1;
			println!("i{} = i{} {} i{}", ret, i, o, value);
			i = ret;
			if let Some(next) = n {
				i = assemble_expr(i, &next);
			}
		},
		Expr::Item(item) => {
			i = i + 1;
			println!("i{} = {}", i, item);
		},
		Expr::Call(_, _) =>
			panic!("cannot handle calls yet"),
	}

	i
}

fn assemble_stmt_expr(e: Box<Expr>)
{
	let r = assemble_expr(0, &e);

	println!("print i{}", r);
}

fn assemble_stmt_list(l: Vec<Stmt>)
{
	for s in l {
		assemble_stmt(s);
	}
}

pub fn assemble_stmt(s: Stmt)
{
	match s {
		Stmt::If(cond, then, otherwise) =>
			panic!("not done"),//assemble_stmt_if(cond, then, otherwise),
		Stmt::Expr(e) =>
			assemble_stmt_expr(e),
		Stmt::List(l) => 
			assemble_stmt_list(l),
	}
}

