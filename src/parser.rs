use std::fmt;
use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
	Unknown,
	None,
	I32,
	I64,
	Array(Box<Type>),
	Ref(Box<Type>),
}

#[derive(Debug)]
pub enum Expr {
	Op(Type, Token, Box<Expr>, Option<Box<Expr>>),
	Start(Type, Box<Expr>, Option<Box<Expr>>),
	Call(String, Vec<Expr>),
	Item(Token),
}

#[derive(Debug)]
pub enum Stmt {
	Return(Option<Box<Expr>>),
	Alloc(String, Type, Box<Expr>),
	Assign(String, Box<Expr>),
	If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
	Expr(Box<Expr>),
	List(Vec<Stmt>),
}

pub struct Fn {
	pub params: Vec<(String, Type)>,
	pub ret: Type,
	pub stmts: Stmt,
}

pub struct Prog {
	pub funcs: Vec<(String, Fn)>,
}

impl fmt::Display for Type {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let s = match self {
			Type::Unknown => format!("_").to_string(),
			Type::None => format!("()").to_string(),
			Type::I64 => format!("i64").to_string(),
			Type::I32 => format!("i32").to_string(),
			Type::Array(t) => format!("[{}]", t).to_string(),
			Type::Ref(t) => format!("&{}", t).to_string(),
		};

		write!(f, "{}", s)
	}
}
	
impl fmt::Display for Expr {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let s = match self {
			Expr::Start(t, v, next) => {
				match next {
					Some(n) =>
						format!("_{} {} {}", t, v, n).to_string(),
					None => 
						format!("_{} {}", t, v).to_string(),
				}
			},
			Expr::Op(t, o, v, next) => {
				match next {
					Some(n) =>
						format!("(_{} {} {} {})", t, o, v, n).to_string(),
					None => 
						format!("(_{} {} {})", t, o, v).to_string(),
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

impl fmt::Display for Stmt {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let s = match self {
			Stmt::Return(value) => {
				match value {
					Some(v) => format!("ret {}", v),
					None => format!("ret"),
				}
			},
			Stmt::Alloc(name, t, value) => {
				format!("alloc {} : {} = {}", name, t, value)
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

impl fmt::Display for Fn {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let ret_s = format!("{}", self.ret).to_string();

		let param_s: Vec<String> = self.params
					.iter()
					.map(|(n, t) | format!("{}: {}", n, t)
						.to_string())
					.collect();

		let s = format!("fn ({}) -> {} {}", 
			param_s.join(", "), 
			ret_s,
			self.stmts);

		write!(f, "{}", s)
	}
}

impl fmt::Display for Prog {
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

fn parse_match(expect: Token, tokens: &mut &[Token]) -> bool
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

fn parse_call(tokens: &mut &[Token]) -> Option<Expr>
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

fn parse_f(tokens: &mut &[Token]) -> Option<Expr>
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
					Some(Expr::Item(t.clone()))
				},
			}
		},
		Token::Integer(_) | Token::Float(_) => {
			*tokens = &tokens[1..];
			Some(Expr::Item(t.clone()))
		},
		tt => panic!("unexpected {}", tt),
	}
}

fn parse_expr_term_p(tokens: &mut &[Token]) -> Option<Expr>
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
			Some(Expr::Op(Type::Unknown, t.clone(), Box::new(v), next))
		},
		_ => None,
	}
}

fn parse_expr_term(tokens: &mut &[Token]) -> Option<Expr>
{
	let v = parse_f(tokens)?;
	let n = match parse_expr_term_p(tokens) {
		Some(nn) => Some(Box::new(nn)),
		None => None,
	};

	Some(Expr::Start(Type::Unknown, Box::new(v), n))
}

fn parse_expr_p(tokens: &mut &[Token]) -> Option<Expr>
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
			Some(Expr::Op(Type::Unknown, t.clone(), Box::new(v), next))
		},
		_ => None,
	}
}

fn parse_expr(tokens: &mut &[Token]) -> Option<Expr>
{
	let v = parse_expr_term(tokens)?;
	let n = match parse_expr_p(tokens) {
		Some(nn) => Some(Box::new(nn)),
		None => None,
	};

	Some(Expr::Start(Type::Unknown, Box::new(v), n))
}
	
fn parse_stmt_expr(tokens: &mut &[Token]) -> Option<Stmt>
{
	let expr = parse_expr(tokens)?;

	if !parse_match(Token::Semicolon, tokens) {
		panic!("expected ;");
	}

	Some(Stmt::Expr(Box::new(expr)))
}

fn parse_stmt_if(tokens: &mut &[Token]) -> Option<Stmt>
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

fn parse_stmt_assign(tokens: &mut &[Token]) -> Option<Stmt>
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

fn parse_stmt_let(tokens: &mut &[Token]) -> Option<Stmt>
{
	if tokens.len() < 3 {
		return None;
	}

	let var_name = match &tokens[0] {
		Token::Symbol(s) => s,
		_ => return None,
	};

	if tokens[1] != Token::Colon {
		return None;
	}

	let mut new_tokens = &tokens[2..];
	
	let var_type = parse_type(&mut new_tokens);
	
	if new_tokens[0] != Token::Assign {
		panic!("expected =");
	}

	*tokens = &new_tokens[1..];

	let var_value = match parse_expr(tokens) {
		None => panic!("expected expression"),
		Some(e) => e,
	};

	if !parse_match(Token::Semicolon, tokens) {
		panic!("expected ;");
	}

	Some(Stmt::Alloc(var_name.to_string(), 
		var_type, 
		Box::new(var_value)))
}

fn parse_stmt_return(tokens: &mut &[Token]) -> Option<Stmt>
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
	
fn parse_stmt_list(tokens: &mut &[Token]) -> Option<Stmt>
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

fn parse_stmt(tokens: &mut &[Token]) -> Option<Stmt>
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

fn parse_type(tokens: &mut &[Token]) -> Type {
	if tokens.len() < 1 {
		panic!("expected type");
	}

	let r = match &tokens[0] {
		Token::Symbol(s) => {
			match s.as_str() {
				"i64" => Type::I64,
				"i32" => Type::I32,
				_ => panic!("expected type, got : {}", 
						&tokens[0]),
			}
		},
		_ => panic!("expected type, got : {}", &tokens[0]),
	};

	*tokens = &tokens[1..];
	r
}

fn parse_fn(tokens: &mut &[Token]) -> Option<(String, Fn)>
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

	let mut param_vec: Vec<(String, Type)> = vec![];

	while tokens.len() > 1 {
		let param_name = match &tokens[0] {
			Token::Symbol(s) => s.to_string(),
			Token::Rparen => break,
			_ => panic!("expected param name"),
		};

		*tokens = &tokens[1..];

		if !parse_match(Token::Colon, tokens) {
			panic!("expected : type");
		}

		let param_type = parse_type(tokens);
		
		param_vec.push((param_name, param_type));

		if !parse_match(Token::Comma, tokens) {
			break;
		}
	}

	if !parse_match(Token::Rparen, tokens) {
		panic!("expected )");
	}

	let ret_type = if parse_match(Token::Arrow, tokens) {
		parse_type(tokens)
	} else {
		Type::None
	};

	let stmts = match parse_stmt_list(tokens) {
		Some(t) => t,
		None => panic!("function expected statement list"),
	};

	let f = Fn {
		params: param_vec,
		ret: ret_type,
		stmts: stmts,
	};
		
	Some((name.to_string(), f))
}

pub fn parse(tokens: &[Token]) -> Prog
{
	let mut mtokens = &tokens[..];

	let mut fns: Vec<(String, Fn)> = vec![];

	while let Some(e) = parse_fn(&mut mtokens) {
		fns.push(e)
	}

	Prog { funcs: fns }
}

