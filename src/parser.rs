use std::fmt;
use crate::lexer::Token;

#[derive(Debug)]
pub enum Expr<'a> {
	Op(&'a Token, Box<Expr<'a>>, Option<Box<Expr<'a>>>),
	Start(Box<Expr<'a>>, Option<Box<Expr<'a>>>),
	Call(String, Vec<Expr<'a>>),
	Item(&'a Token),
}

#[derive(Debug)]
pub enum Stmt<'a> {
	Return(Option<Box<Expr<'a>>>),
	Alloc(String, Box<Expr<'a>>),
	Assign(String, Box<Expr<'a>>),
	If(Box<Expr<'a>>, Box<Stmt<'a>>, Option<Box<Stmt<'a>>>),
	Expr(Box<Expr<'a>>),
	List(Vec<Stmt<'a>>),
}

pub struct Fn<'a> {
	pub params: Vec<String>,
	pub stmts: Stmt<'a>,
}

pub struct Prog<'a> {
	pub funcs: Vec<(String, Fn<'a>)>,
}

impl<'a> fmt::Display for Expr<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let s = match self {
			Expr::Start(v, next) => {
				match next {
					Some(n) =>
						format!("{} {}", v, n).to_string(),
					None => 
						format!("{}", v).to_string(),
				}
			},
			Expr::Op(o, v, next) => {
				match next {
					Some(n) =>
						format!("({} {} {})", o, v, n).to_string(),
					None => 
						format!("({} {})", o, v).to_string(),
				}
			},
			Expr::Call(f, args) => {
				let xs: Vec<String> = args
					.iter()
					.map(|x| x.to_string())
					.collect();
				format!("[call {} : {}]", f, xs.join(", ")).to_string()
			},
			Expr::Item(t) => format!("{}", t).to_string(),
		};

		write!(f, "{}", s)
	}
}

impl<'a> fmt::Display for Stmt<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let s = match self {
			Stmt::Return(value) => {
				match value {
					Some(v) => format!("ret {}", v),
					None => format!("ret"),
				}
			},
			Stmt::Alloc(name, value) => {
				format!("alloc {} = {}", name, value)
			},
			Stmt::Assign(name, value) => {
				format!("assign {} = {}", name, value)
			},
			Stmt::If(cond, then, otherwise) => {
				match otherwise {
					Some(o) => format!("if ({}) then {} else {}", cond, then, o).to_string(),
					None => format!("if ({}) then {}", cond, then).to_string(),
				}
			},
			Stmt::Expr(e) => {
				format!("expr {};", e).to_string()
			},
			Stmt::List(stmts) => {
				let xs: Vec<String> = stmts
					.iter()
					.map(|x| x.to_string())
					.collect();
				format!("{{\n{}\n}}", xs.join("\n"))
			},
		};

		write!(f, "{}", s)
	}
}

impl<'a> fmt::Display for Fn<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let param_s: Vec<String> = self.params
					.iter()
					.map(|x| x.to_string())
					.collect();

		let s = format!("fn ({}) {}", 
			param_s.join(", "), self.stmts);

		write!(f, "{}", s)
	}
}

impl<'a> fmt::Display for Prog<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let fn_s: Vec<String> = self.funcs
					.iter()
					.map(|(n, f)| format!("{} {}", 
						n.to_string(), f))
					.collect();

		let s = format!("prog:\n{}",
			fn_s.join("\n"));

		write!(f, "{}", s)
	}
}

fn parse_match<'a>(expect: Token, tokens: &mut &'a [Token]) -> bool
{
	if tokens.len() == 0 {
		false
	} else if tokens[0] == expect {
		*tokens = &tokens[1..];
		true
	} else {
		false
	}
}

fn parse_call<'a>(tokens: &mut &'a [Token]) -> Option<Expr<'a>>
{
	if tokens.len() < 2 {
		return None;
	}

	let t = &tokens[0];
	let n = &tokens[1];

	match (t, n) {
		(Token::Symbol(s), Token::Lparen) => {
			*tokens = &tokens[2..];
			let mut arg_vec: Vec<Expr> = vec![];

			while let Some(e) = parse_expr(tokens) {
				arg_vec.push(e);
				if !parse_match(Token::Comma, tokens) {
					break;
				}
			}

			if parse_match(Token::Rparen, tokens) {
				Some(Expr::Call(s.to_string(), arg_vec))
			} else {
				panic!("expected )");
			}
		},
		_ => None,
	}
}

fn parse_f<'a>(tokens: &mut &'a [Token]) -> Option<Expr<'a>>
{
	let t = &tokens[0];

	match t {
		Token::Lparen => {
			*tokens = &tokens[1..];
			let r = parse_expr(tokens);
			if parse_match(Token::Rparen, tokens) {
				r
			} else {
				panic!("expected )");
			}
		},
		Token::Symbol(_) => {
			match parse_call(tokens) {
				Some(c) => Some(c),
				None => {
					*tokens = &tokens[1..];
					Some(Expr::Item(t))
				},
			}
		},
		Token::Integer(_) | Token::Float(_) => {
			*tokens = &tokens[1..];
			Some(Expr::Item(t))
		},
		tt => panic!("unexpected {}", tt),
	}
}

fn parse_expr_term_p<'a>(tokens: &mut &'a [Token]) -> Option<Expr<'a>>
{
	let t = &tokens[0];
	match t {
		Token::OpMul | Token::OpDiv | Token::OpRem => {
			*tokens = &tokens[1..];
			let v = parse_f(tokens)?;
			let next = match parse_expr_term_p(tokens) {
				Some(n) => Some(Box::new(n)),
				None => None,
			};
			Some(Expr::Op(t, Box::new(v), next))
		},
		_ => None,
	}
}

fn parse_expr_term<'a>(tokens: &mut &'a [Token]) -> Option<Expr<'a>>
{
	let v = parse_f(tokens)?;
	let n = match parse_expr_term_p(tokens) {
		Some(nn) => Some(Box::new(nn)),
		None => None,
	};

	Some(Expr::Start(Box::new(v), n))
}

fn parse_expr_p<'a>(tokens: &mut &'a [Token]) -> Option<Expr<'a>>
{
	let t = &tokens[0];
	match t {
		Token::OpAdd | Token::OpSub => {
			*tokens = &tokens[1..];
			let v = parse_expr_term(tokens)?;
			let next = match parse_expr_p(tokens) {
				Some(n) => Some(Box::new(n)),
				None => None,
			};
			Some(Expr::Op(t, Box::new(v), next))
		},
		_ => None,
	}
}

fn parse_expr<'a>(tokens: &mut &'a [Token]) -> Option<Expr<'a>>
{
	let v = parse_expr_term(tokens)?;
	let n = match parse_expr_p(tokens) {
		Some(nn) => Some(Box::new(nn)),
		None => None,
	};

	Some(Expr::Start(Box::new(v), n))
}
	
fn parse_stmt_expr<'a>(tokens: &mut &'a [Token]) -> Option<Stmt<'a>>
{
	let expr = parse_expr(tokens)?;

	if !parse_match(Token::Semicolon, tokens) {
		panic!("expected ;");
	}

	Some(Stmt::Expr(Box::new(expr)))
}

fn parse_stmt_if<'a>(tokens: &mut &'a [Token]) -> Option<Stmt<'a>>
{
	if tokens[0] != Token::If {
		return None;
	}

	*tokens = &tokens[1..];

	let cond = match parse_expr(tokens) {
		None => panic!("expected expression"),
		Some(e) => e,
	};
	
	let then = match parse_stmt_list(tokens) {
		None => panic!("expected statements"),
		Some(s) => s,
	};

	let otherwise = 
		if tokens.len() > 0 && tokens[0] == Token::Else {
			*tokens = &tokens[1..];
			match parse_stmt_list(tokens) {
				None => panic!("expected statements"),
				Some(s) => Some(Box::new(s)),
			}
		} else {
			None
		};

	Some(Stmt::If(Box::new(cond), Box::new(then), otherwise))
}

fn parse_stmt_assign<'a>(tokens: &mut &'a [Token]) -> Option<Stmt<'a>>
{
	if tokens.len() < 2 {
		return None;
	}

	let name = match &tokens[0] {
		Token::Symbol(s) => s,
		_ => return None,
	};

	if tokens[1] != Token::Assign {
		return None;
	}
	
	*tokens = &tokens[2..];

	let value = match parse_expr(tokens) {
		None => panic!("expected expression"),
		Some(e) => e,
	};

	if !parse_match(Token::Semicolon, tokens) {
		panic!("expected ;");
	}

	Some(Stmt::Assign(name.to_string(), Box::new(value)))
}

fn parse_stmt_let<'a>(tokens: &mut &'a [Token]) -> Option<Stmt<'a>>
{
	if tokens.len() < 3 {
		return None;
	}

	let name = match &tokens[0] {
		Token::Symbol(s) => s,
		_ => return None,
	};

	if tokens[1] != Token::Colon {
		return None;
	}
	
	if tokens[2] != Token::Assign {
		return None;
	}
	
	*tokens = &tokens[3..];

	let value = match parse_expr(tokens) {
		None => panic!("expected expression"),
		Some(e) => e,
	};

	if !parse_match(Token::Semicolon, tokens) {
		panic!("expected ;");
	}

	Some(Stmt::Alloc(name.to_string(), Box::new(value)))
}

fn parse_stmt_return<'a>(tokens: &mut &'a [Token]) -> Option<Stmt<'a>>
{
	if !parse_match(Token::Return, tokens) {
		return None;
	}

	if tokens.len() < 1 {
		return Some(Stmt::Return(None));
	}

	let e = match parse_expr(tokens) {
		Some(ee) => Some(Box::new(ee)),
		None => None,
	};

	if !parse_match(Token::Semicolon, tokens) {
		panic!("expected ;");
	}

	return Some(Stmt::Return(e));
}
	
fn parse_stmt_list<'a>(tokens: &mut &'a [Token]) -> Option<Stmt<'a>>
{
	let mut stmts: Vec<Stmt> = vec![];

	if !parse_match(Token::Lbrace, tokens) {
		return None;
	}

	while let Some(e) = parse_stmt(tokens) {
		stmts.push(e);
		if tokens[0] == Token::Rbrace {
			break;
		}
	}

	if !parse_match(Token::Rbrace, tokens) {
		panic!("expected }");
	}

	Some(Stmt::List(stmts))
}

fn parse_stmt<'a>(tokens: &mut &'a [Token]) -> Option<Stmt<'a>>
{
	if tokens.len() == 0 {
		return None
	}

	parse_stmt_list(tokens)
		.or_else(|| parse_stmt_return(tokens))
		.or_else(|| parse_stmt_if(tokens))
		.or_else(|| parse_stmt_assign(tokens))
		.or_else(|| parse_stmt_let(tokens))
		.or_else(|| parse_stmt_expr(tokens))
}

fn parse_fn<'a>(tokens: &mut &'a [Token]) -> Option<(String, Fn<'a>)>
{
	if !parse_match(Token::Fn, tokens) {
		return None;
	}

	if tokens.len() < 2 {
		panic!("expected fn name");
	}

	let name = match &tokens[0] {
		Token::Symbol(s) => s,
		_ => panic!("expected fn name"),
	};

	*tokens = &tokens[1..];

	if !parse_match(Token::Lparen, tokens) {
		panic!("unexpected {} expected (", &tokens[0]);
	}

	let mut param_vec: Vec<String> = vec![];

	while tokens.len() > 1 {
		let n = match &tokens[0] {
			Token::Symbol(s) => s.to_string(),
			Token::Rparen => break,
			_ => panic!("expected param name"),
		};

		*tokens = &tokens[1..];

		param_vec.push(n);
		if !parse_match(Token::Comma, tokens) {
			break;
		}
	}

	if !parse_match(Token::Rparen, tokens) {
		panic!("expected )");
	}

	let stmts = match parse_stmt_list(tokens) {
		Some(t) => t,
		None => panic!("function expected statement list"),
	};

	let f = Fn {
		params: param_vec,
		stmts: stmts,
	};
		
	Some((name.to_string(), f))
}

pub fn parse<'a>(tokens: &'a [Token]) -> Prog<'a>
{
	let mut mtokens = &tokens[..];

	let mut fns: Vec<(String, Fn)> = vec![];

	while let Some(e) = parse_fn(&mut mtokens) {
		fns.push(e)
	}

	Prog { funcs: fns }
}

