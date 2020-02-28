use std::env;
use std::fs;

pub mod lexer;
pub mod parser;
pub mod assembler;

fn main() {
	let args = env::args().collect::<Vec<_>>();

    if args.len() == 2 {
    	let path = &args[1];
    	let input = fs::read_to_string(path).expect("cant read file");

		let tokens = lexer::tokenize(input);
    	println!("tokens {:?}", tokens);
		let parsed = parser::parse(&tokens);
    	println!("stmts {}", parsed);
		//let ops = 
		assembler::assemble_stmt(parsed);
    } else {
    	println!("expected a file");
    }
}

