use std::env;
use std::fs;

pub mod lexer;
pub mod parser;
pub mod assembler;
pub mod evaluator;

fn main() {
	let args = env::args().collect::<Vec<_>>();

    if args.len() == 2 {
    	let path = &args[1];
    	let input = fs::read_to_string(path).expect("cant read file");

		let tokens = lexer::tokenize(input);
    	println!("tokens {:?}", tokens);

		let parsed = parser::parse(&tokens);
    	println!("stmts {}", parsed);

		let ops = assembler::assemble(parsed);
    	println!("ops:");
    	for o in &ops {
    		println!("{}", o);
    	}

		evaluator::eval(ops);
    } else {
    	println!("expected a file");
    }
}

