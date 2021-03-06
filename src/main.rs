use std::env;
use std::fs;

pub mod lexer;
pub mod parser;
pub mod typechecker;
pub mod assembler;
pub mod evaluator;

fn main() {
	let args = env::args().collect::<Vec<_>>();

    if args.len() == 2 {
    	let path = &args[1];
    	let input = fs::read_to_string(path).expect("cant read file");

		let tokens = lexer::tokenize(input);

		let parsed = parser::parse(&tokens);
    	println!("prog {}", parsed);
    	println!("");

    	let checked = typechecker::typecheck(parsed);

		let ops = assembler::assemble(checked);
    	println!("ops:");
    	println!("");
    	let mut i = 0;
    	for o in &ops {
    		println!("{}: {}", i, o);
    		i += 1;
    	}

    	println!("");

		evaluator::eval(ops);

    } else {
    	println!("expected a file");
    }
}

