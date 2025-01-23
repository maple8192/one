use std::{env, fs};

use lexer::Lexer;
use parser::Parser;
use vm::VM;

mod ast;
mod lexer;
mod parser;
mod scanner;
mod token;
mod vm;

fn main() {
    let Some(src_path) = env::args().nth(1) else {
        eprintln!("src file path is required");
        return;
    };
    let src = fs::read_to_string(src_path).unwrap();

    let lexer = Lexer::new(&src);
    let mut parser = Parser::new(lexer);
    let program = match parser.parse() {
        Ok(program) => program,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };
    let mut vm = VM::new();
    match vm.run(&program) {
        Ok(_) => {}
        Err(e) => eprintln!("{}", e),
    }
}
