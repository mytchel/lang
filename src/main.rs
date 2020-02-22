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
	Assign,
	Integer(i64),
	Float(f64),
	Bool(bool),
	Symbol(String),
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
			Token::Assign => "=".to_string(),
			Token::Integer(i) => i.to_string(),
			Token::Float(i) => i.to_string(),
			Token::Bool(i) => i.to_string(),
			Token::Symbol(i) => i.clone(),
		};

		write!(f, "{}", str)
	}
}

struct Reader<'a> {
	input: &'a Vec<char>,
	index: usize,
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
			if c.is_whitespace() {
				false

			} else if c == '(' 
					|| c == ')'
					|| c == '['
					|| c == ']'
					|| c == '{'
					|| c == '}'
			{
				false

			} else if c == '*'
					|| c == '/'
					|| c == '+'
					|| c == '-'
					|| c == '>'
					|| c == '<'
					|| c == '='
					|| c == '!'
					|| c == '@'
					|| c == '#'
					|| c == '$'
					|| c == '%'
					|| c == '^'
					|| c == '&'
			{
				false

			} else {
				true
			}
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
			r.take(1, |_, _| true);
		}
		true
	} else {
		false
	}
}

fn parse_brace(r: &mut Reader) -> Option<Token> {
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
	} else {
		None
	}
}

fn parse_operator(r: &mut Reader) -> Option<Token> {
	if r.take_match("*") {
		Some(Token::OpMul)
	} else if r.take_match("/") {
		Some(Token::OpDiv)
	} else if r.take_match("+") {
		Some(Token::OpAdd)
	} else if r.take_match("-") {
		Some(Token::OpSub)
	} else if r.take_match("%") {
		Some(Token::OpRem)
	} else if r.take_match("==") {
		Some(Token::CompEqual)
	} else if r.take_match("!=") {
		Some(Token::CompInEqual)
	} else if r.take_match(">=") {
		Some(Token::CompGreaterEqual)
	} else if r.take_match("<=") {
		Some(Token::CompLessEqual)
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

	parse_brace(r)
		.or_else(|| parse_operator(r))
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

fn parse(tokens: Vec<Token>) -> bool {
	false
}

fn main() {
	let args = env::args().collect::<Vec<_>>();

    println!("Hello, world!");

    if args.len() == 2 {
    	let path = &args[1];
    	let input = fs::read_to_string(path).expect("cant read file");

		let tokens = tokenize(input);
    	println!("tokens '{:?}'", tokens);
		let parsed = parse(tokens);
    } else {
    	println!("expected a file");
    }
}
