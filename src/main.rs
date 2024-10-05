use lalrpop_util::lalrpop_mod;

mod ast;
lalrpop_mod!(pub grammar);

use std::{env, fs, process};

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
        let _ = run(src);
    } else {
        todo!("repl");
    }
}

fn run(src: String) -> Result<(), ()> {
    let ast = match grammar::ProgramParser::new().parse(&src) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("{e}");
            return Err(());
        }
    };
    println!("{:?}", ast);
    Ok(())
}