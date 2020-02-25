use std::env;
use std::fs;
use std::fmt;

#[derive(Debug)]
enum Token {
	Lparen,
	Rparen,
	Lbrace,
	Rbrace,
	Lsquare,
	Rsquare,
	Comma,
	Semicolon,
	Colon,
	Ref,
	Pipe,
	OpMul,
	OpDiv,
	OpAdd,
	OpSub,
	OpRem,
	CompEqual,
	CompInEqual,
	CompGreaterEqual,
	CompGreater,
	CompLessEqual,
	CompLess,
	CompAnd,
	CompOr,
	Assign,
	Integer(i64),
	Float(f64),
	Bool(bool),
	String(String),
	Symbol(String),
}

struct Reader<'a> {
	input: &'a Vec<char>,
	index: usize,
}

enum Expr<'a> {
	O(&'a Token, Box<Expr<'a>>, Box<Expr<'a>>),
	B(Box<Expr<'a>>, Box<Expr<'a>>),
	F(&'a Token),
	N,
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let str = match self {
			Token::Lparen => "(".to_string(),
			Token::Rparen => ")".to_string(),
			Token::Lbrace => "{".to_string(),
			Token::Rbrace => "}".to_string(),
			Token::Lsquare => "[".to_string(),
			Token::Rsquare => "]".to_string(),
			Token::Semicolon => ";".to_string(),
			Token::Colon => ":".to_string(),
			Token::Comma => ",".to_string(),
			Token::Ref => "&".to_string(),
			Token::Pipe => "|".to_string(),
			Token::OpMul => "*".to_string(),
			Token::OpDiv => "/".to_string(),
			Token::OpAdd => "+".to_string(),
			Token::OpSub => "-".to_string(),
			Token::OpRem => "%".to_string(),
			Token::CompEqual => "==".to_string(),
			Token::CompInEqual => "!=".to_string(),
			Token::CompGreaterEqual => ">=".to_string(),
			Token::CompLessEqual => "<=".to_string(),
			Token::CompGreater => ">".to_string(),
			Token::CompLess => "<".to_string(),
			Token::CompAnd => "&&".to_string(),
			Token::CompOr => "||".to_string(),
			Token::Assign => "=".to_string(),
			Token::Integer(i) => i.to_string(),
			Token::Float(i) => i.to_string(),
			Token::Bool(i) => i.to_string(),
			Token::String(i) => i.clone(),
			Token::Symbol(i) => i.clone(),
		};

		write!(f, "{}", str)
	}
}

impl<'a> fmt::Display for Expr<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let str = match self {
			Expr::F(t) => format!("{}", t).to_string(),
			Expr::O(o, e1, e2) => format!(" {} {}{}", o, e1, e2).to_string(),
			Expr::B(e1, e2) => format!("({}{})", e1, e2).to_string(),
			Expr::N => "".to_string(),
		};

		write!(f, "{}", str)
	}
}

impl Reader<'_> {
	fn new<'a>(i: &'a Vec<char>) -> Reader {
		Reader {
			input: i,
			index: 0,
		}
	}

	fn empty(&self) -> bool {
		self.index + 1 >= self.input.len()
	}
	
	fn take<F>(&mut self, len: usize, f: F) -> Vec<char>
		where F: Fn(usize, char) -> bool
	{
		let mut i = 0;
		let mut vec: Vec<char> =  Vec::new();

		if self.index + len >= self.input.len() {
			return vec;
		}

		while self.index + i < self.input.len() {
			let c = self.input[self.index + i];
			if !f(i, c) {
				break;
			}

			vec.push(c);
			i += 1;

			if len > 0 && i == len {
				break;
			}
		}

		if len > 0 && i < len {
			Vec::new()
		} else {
			self.index += i;
			vec
		}
	}

	fn take_match(&mut self, s: &str) -> bool {
		let v: Vec<char> = s.chars().collect();
		self.take(v.len(), |i, c| v[i] == c).len() > 0
	}

	fn take_next(&mut self) -> Vec<char> {
		fn f (_i: usize, c: char) -> bool {
			c.is_alphanumeric() || c == '.' || c == '_'
		}

		self.take(0, f)
	}
}

fn parse_whitespace(r: &mut Reader) -> bool {
	r.take(1, |_, c| c.is_whitespace()).len() > 0 
}

fn parse_comment(r: &mut Reader) -> bool {
	if r.take_match("//") {
		r.take(0, |_, c| c != '\n');
		true
	} else if r.take_match("/*") {
		while !r.take_match("*/") {
			if r.take(1, |_, _| true).len() == 0 {
				panic!("comment end not found!");
			}
		}
		true
	} else {
		false
	}
}

fn parse_syntax(r: &mut Reader) -> Option<Token> {
	/* braces */
	if r.take_match("(") {
		Some(Token::Lparen)
	} else if r.take_match(")") {
		Some(Token::Rparen)
	} else if r.take_match("{") {
		Some(Token::Lbrace)
	} else if r.take_match("}") {
		Some(Token::Rbrace)
	} else if r.take_match("[") {
		Some(Token::Lsquare)
	} else if r.take_match("]") {
		Some(Token::Rsquare)

	/* double character */
	} else if r.take_match("==") {
		Some(Token::CompEqual)
	} else if r.take_match("!=") {
		Some(Token::CompInEqual)
	} else if r.take_match(">=") {
		Some(Token::CompGreaterEqual)
	} else if r.take_match("<=") {
		Some(Token::CompLessEqual)
	} else if r.take_match("&&") {
		Some(Token::CompAnd)
	} else if r.take_match("||") {
		Some(Token::CompOr)

	} else if r.take_match(";") {
		Some(Token::Semicolon)
	} else if r.take_match(":") {
		Some(Token::Colon)
	} else if r.take_match(",") {
		Some(Token::Comma)
	} else if r.take_match("&") {
		Some(Token::Ref)
	} else if r.take_match("|") {
		Some(Token::Pipe)

	} else if r.take_match("*") {
		Some(Token::OpMul)
	} else if r.take_match("/") {
		Some(Token::OpDiv)
	} else if r.take_match("+") {
		Some(Token::OpAdd)
	} else if r.take_match("-") {
		Some(Token::OpSub)
	} else if r.take_match("%") {
		Some(Token::OpRem)

	} else if r.take_match(">") {
		Some(Token::CompGreater)
	} else if r.take_match("<") {
		Some(Token::CompLess)
	} else if r.take_match("=") {
		Some(Token::Assign)
	} else {
		None
	}
}

fn parse_string(r: &mut Reader) -> Option<Token> {
	if r.take_match("\"") {
		let mut v: Vec<char> = Vec::new();
		while !r.take_match("\"") {
			let g = r.take(1, |_, _| true);
			if g.len() == 1 {
				v.push(g[0])
			} else {
				panic!("string end not found!");
			}
		}

		let s: String = v.into_iter().collect();
		Some(Token::String(s))
	} else {
		None
	}
}

fn parse_binary(v: Vec<char>) -> Option<Token> {
	let s: String = v.iter().collect();

	let mut sum: i64 = 0;

	for i in &v[2..] {
		match i.to_digit(2) {
			Some(d) => sum = sum * 2 + d as i64,
			None => if *i !=  '_' {
				panic!("bad formed binary number {}", s);
			},
		}
	}

	Some(Token::Integer(sum))
}

fn parse_hex(v: Vec<char>) -> Option<Token> {
	let s: String = v.iter().collect();

	let mut sum: i64 = 0;

	for i in &v[2..] {
		match i.to_digit(16) {
			Some(d) => sum = sum * 16 + d as i64,
			None => if *i !=  '_' {
				panic!("bad formed hex number {}", s);
			},
		}
	}

	Some(Token::Integer(sum))
}

fn parse_number(v: Vec<char>) -> Option<Token> {
	let s: String = v.iter().collect();
	let mut p: Vec<char> =  Vec::new();

	for i in v {
		if i.is_digit(10) || i == '.' {
			p.push(i)
		} else if i != '_' {
			panic!("bad formed number {}", s);
		}
	}

	let cleaned: String = p.into_iter().collect();
	cleaned.parse::<i64>().map(Token::Integer)
		.or_else(|_| cleaned.parse::<f64>().map(Token::Float))
		.ok()
}

fn parse_piece(r: &mut Reader) -> Option<Token> {
	let v = r.take_next();
	println!("got {:?}", v);

	if v.len() == 0 {
		None

	} else if v[0] == '0' && v.len() > 2 && v[1] == 'b' {
		parse_binary(v)

	} else if v[0] == '0' && v.len() > 2 && v[1] == 'x' {
		parse_hex(v)

	} else if v[0].is_digit(10) {
		parse_number(v)

	} else if v[0].is_alphabetic() || v[0] == '_' {
		let s: String = v.into_iter().collect();

		/* TODO: make sure there are no .'s in it */

		println!("symbol = {}", s);

		match s.as_str() {
			"true" => Some(Token::Bool(true)),
			"false" => Some(Token::Bool(false)),
			_ => Some(Token::Symbol(s)),
		}

	} else {
		let s: String = v.into_iter().collect();
		panic!("cannot parse {}", s);
	}
}

fn tokenize_single(r: &mut Reader) -> Option<Token>
{
	while parse_whitespace(r) || parse_comment(r) {
		continue;
	}

	parse_syntax(r)
		.or_else(|| parse_string(r))
		.or_else(|| parse_piece(r))
		.or_else(|| {
			if r.empty() { 
				println!("empty");
				None 
			}  else {
				panic!("syntax error at index {} of {}", r.index, r.input.len())
			}
		})
}

fn tokenize(input: String) -> Vec<Token> {
	let v = input.chars().collect();
	let mut r = Reader::new(&v);

	let mut tokens: Vec<Token> = vec![];

	println!("tokenize");

	while let Some(x) = tokenize_single(&mut r) {
		tokens.push(x)
	}

	tokens
}

fn parse_match<'a>(expect: Token, tokens: &mut &'a [Token]) -> bool
{
	let t = &tokens[0];
	match t {
		expect => {
			*tokens = &tokens[1..];
			true
		},
		_ => false
	}
}

fn parse_f<'a>(tokens: &mut &'a [Token]) -> Option<Expr<'a>>
{
	let t = &tokens[0];

	match t {
		Token::Lparen => {
			*tokens = &tokens[1..];
			let r = parse_expr(tokens);
			if !parse_match(Token::Rparen, tokens) {
				panic!("expected )");
			} else {
				r
			}
		},
		Token::Integer(i) => {
			*tokens = &tokens[1..];
			print!(" {} ", i);
			Some(Expr::F(t))
		}
		tt => panic!("unexpected {}", tt),
	}
}

fn parse_term_p<'a>(tokens: &mut &'a [Token]) -> Option<Expr<'a>>
{
	let t = &tokens[0];

	match t {
		Token::OpMul => {
			*tokens = &tokens[1..];
			print!("*");
			let f = parse_f(tokens)?;
			let e = match parse_term_p(tokens) {
				Some(term) => term,
				None => Expr::N,
			};
			Some(Expr::O(t, Box::new(f), Box::new(e)))
		},
		_ => None,
	}
}

fn parse_term<'a>(tokens: &mut &'a [Token]) -> Option<Expr<'a>>
{
	print!("(");
	
	let f = parse_f(tokens)?;;
	let e = match parse_term_p(tokens) {
		Some(term) => term,
		None => Expr::N,
	};

	print!(")");
	Some(Expr::B(Box::new(f), Box::new(e)))
}

fn parse_expr_p<'a>(tokens: &mut &'a [Token]) -> Option<Expr<'a>>
{
	let t = &tokens[0];
	match t {
		Token::OpAdd => {
			*tokens = &tokens[1..];
			print!("+");
			let f = parse_term(tokens)?;
			let e = match parse_expr_p(tokens) {
				Some(expr) => expr,
				None => Expr::N,
			};

			Some(Expr::O(t, Box::new(f), Box::new(e)))
		},
		_ => None,
	}
}

fn parse_expr<'a>(tokens: &mut &'a [Token]) -> Option<Expr<'a>>
{
	if tokens.len() == 0 {
		return None
	}

	let f = parse_term(tokens)?;
	let e = match parse_expr_p(tokens) {
		Some(expr) => expr,
		None => Expr::N,
	};
	
	Some(Expr::B(Box::new(f), Box::new(e)))
}

fn parse_stmt<'a>(tokens: &mut &'a [Token]) -> Option<Expr<'a>>
{
	if tokens.len() == 0 {
		return None
	}

	let r = parse_expr(tokens);

	if !parse_match(Token::Semicolon, tokens) {
		panic!("expected )");
	} else {
		r
	}
}

fn parse<'a>(tokens: &'a [Token]) -> Vec<Expr<'a>>
{
	println!("parse tokens");

	let mut mtokens = &tokens[..];

	let mut exprs: Vec<Expr> = vec![];

	while let Some(e) = parse_stmt(&mut mtokens) {
		println!("have expression: {}", e);
		exprs.push(e)
	}

	exprs
}

fn main() {
	let args = env::args().collect::<Vec<_>>();

    println!("Hello, world!");

    if args.len() == 2 {
    	let path = &args[1];
    	let input = fs::read_to_string(path).expect("cant read file");

		let tokens = tokenize(input);
    	println!("tokens '{:?}'", tokens);
		let parsed = parse(&tokens);
    } else {
    	println!("expected a file");
    }
}
