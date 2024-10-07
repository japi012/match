use std::path::Path;
use crate::{interpreter::RuntimeError, parser::SyntaxError};

fn line_and_column(src: &str, pos: usize) -> (usize, usize) {
    let mut line = 1;
    let mut column = 1;

    for (char_index, c) in src.chars().enumerate() {
        if char_index == pos {
            return (line, column);
        }

        match c {
            '\n' => {
                line += 1;
                column = 1;
            }
            _ => column += 1,
        }
    }

    (line, column)
}

pub fn display_syntax_err(err: SyntaxError, path: &Path, src: &str) -> String {
    let (line, column) = line_and_column(
        src,
        match err {
            SyntaxError::Expected { loc, .. }
            | SyntaxError::Unexpected(_, loc)
            | SyntaxError::CouldntFindInclude(_, loc) => loc,
        },
    );

    let mut displayed = String::new();
    displayed.push_str(&format!("{}:{line}:{column}: syntax error: ", path.display()));
    displayed.push_str(&match err {
        SyntaxError::Expected {
            expected, found, ..
        } => format!("expected {expected}, found {}", found.token),
        SyntaxError::Unexpected(lexeme, _) => format!("unexpected {}", lexeme.token),
        SyntaxError::CouldntFindInclude(filename, _) => {
            format!("couldn't find file `{}`", filename.display())
        }
    });
    displayed
}

pub fn display_runtime_err(err: RuntimeError, path: &str, src: &str) -> String {
    let (line, column) = line_and_column(
        src,
        match err {
            RuntimeError::CallNonFunc(_, loc)
            | RuntimeError::DivisionByZero(loc)
            | RuntimeError::DoesntSupportTypes(_, _, _, loc)
            | RuntimeError::Explicit(loc)
            | RuntimeError::UndefinedFunc(_, loc)
            | RuntimeError::UnmatchedValue(_, loc)
            | RuntimeError::GuardIsNotBool(_, loc) => loc,
        },
    );

    let mut displayed = String::new();
    displayed.push_str(&format!("{path}:{line}:{column}: runtime error: "));
    displayed.push_str(&match err {
        RuntimeError::CallNonFunc(f, _) => format!("tried to call `{f}`"),
        RuntimeError::DivisionByZero(_) => "division by zero".to_string(),
        RuntimeError::DoesntSupportTypes(l, r, op, _) => format!("tried to {op} `{l}` and `{r}`."),
        RuntimeError::Explicit(_) => "explicit".to_string(),
        RuntimeError::UndefinedFunc(n, _) => format!("undefined variable `{n}`"),
        RuntimeError::UnmatchedValue(v, _) => format!("unmatched value `{v}`"),
        RuntimeError::GuardIsNotBool(v, _) => format!("guard should be a boolean, got `{v}`"),
    });
    displayed
}
