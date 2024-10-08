use err::{display_runtime_err, display_syntax_err};
use interpreter::Interpreter;
use parser::Parser;

mod err;
mod interpreter;
mod lexer;
mod parser;

use std::{env, fs, path::Path, process};

fn main() {
    let mut args = env::args();
    let _program_name = args.next().unwrap();
    if let Some(filename) = args.next() {
        let src = match fs::read_to_string(&filename) {
            Ok(src) => src,
            Err(e) => {
                eprintln!("couldn't open file `{filename}`: {e}");
                process::exit(1);
            }
        };
        let mut interpreter = Interpreter::default();
        let _ = run(src, &mut interpreter, args.collect(), &filename);
    } else {
        eprintln!("provide a filepath as an argument");
    }
}

fn run(
    src: String,
    interpreter: &mut Interpreter,
    args: Vec<String>,
    path: &str,
) -> Result<(), ()> {
    let program = Parser::new(&src)
        .program()
        .map(|p| p.flatten_includes(Path::new(path).to_owned(), &mut Vec::new()))
        .map_err(|e| eprintln!("{}", display_syntax_err(e, Path::new(path), &src)))?;
    let ast = program.map_err(|(p, e)| eprintln!("{}", display_syntax_err(e, &p, &src)))?;
    interpreter.register(ast);
    interpreter
        .run(args)
        .map_err(|e| eprintln!("{}", display_runtime_err(e, path, &src)))?;
    Ok(())
}
