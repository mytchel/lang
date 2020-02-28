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
	If(Box<Expr<'a>>, Box<Stmt<'a>>, Option<Box<Stmt<'a>>>),
	Expr(Box<Expr<'a>>),
	List(Vec<Stmt<'a>>),
}

impl<'a> fmt::Display for Expr<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let str = match self {
			Expr::Start(v, next) => {
				match next {
					Some(n) =>
						format!("[{} {}]", v, n).to_string(),
					None => 
						format!("[{}]", v).to_string(),
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

		write!(f, "{}", str)
	}
}

impl<'a> fmt::Display for Stmt<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let str = match self {
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

		write!(f, "{}", str)
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
	
	let then = match parse_stmts(tokens) {
		None => panic!("expected statements"),
		Some(s) => s,
	};

	let otherwise = 
		if tokens.len() > 0 && tokens[0] == Token::Else {
			*tokens = &tokens[1..];
			match parse_stmts(tokens) {
				None => panic!("expected statements"),
				Some(s) => Some(Box::new(s)),
			}
		} else {
			None
		};

	Some(Stmt::If(Box::new(cond), Box::new(then), otherwise))
}

fn parse_stmts<'a>(tokens: &mut &'a [Token]) -> Option<Stmt<'a>>
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

	parse_stmt_if(tokens)
		.or_else(|| parse_stmts(tokens))
		.or_else(|| parse_stmt_expr(tokens))
}

pub fn parse<'a>(tokens: &'a [Token]) -> Stmt<'a>
{
	let mut mtokens = &tokens[..];

	let mut stmts: Vec<Stmt> = vec![];

	while let Some(e) = parse_stmt(&mut mtokens) {
		stmts.push(e)
	}

	Stmt::List(stmts)
}

