use std::{
    fmt, fs,
    path::{self, PathBuf},
};

use crate::lexer::{Lexeme, Lexer, Token};

#[derive(Debug, Clone)]
pub struct Program {
    pub defs: Vec<Def>,
    pub includes: Vec<(PathBuf, usize)>,
}

impl Program {
    pub fn flatten_includes(
        self,
        path: PathBuf,
        opened_files: &mut Vec<PathBuf>,
    ) -> Result<Vec<Def>, (PathBuf, SyntaxError)> {
        let mut new_defs = self.defs;
        for (include, loc) in self.includes {
            if !opened_files.contains(&include) {
                let Ok(src) = fs::read_to_string(&include) else {
                    return Err((path, SyntaxError::CouldntFindInclude(include, loc)));
                };
                let absolute = path::absolute(&include)
                    .unwrap_or_else(|_| panic!("couldn't find absolute path"));
                opened_files.push(absolute);
                let program = Parser::new(&src)
                    .program()
                    .map_err(|e| (include.clone(), e))?;
                let mut defs = program.flatten_includes(include, opened_files)?;
                new_defs.append(&mut defs);
            }
        }
        Ok(new_defs)
    }
}

#[derive(Debug, Clone)]
pub struct Def {
    pub name: String,
    pub branches: Vec<(Pattern, Expr)>,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub src: PatternSource,
    pub guard: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum PatternSource {
    Number(f64),
    Char(char),
    Named(String),
    Cons(Box<PatternSource>, Box<PatternSource>),
    Wildcard,
    Bool(bool),
    Nil,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f64, usize),
    Named(String, usize),
    Char(char, usize),
    String(String, usize),
    Nil(usize),
    Bool(bool, usize),
    Binary {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op: Op,
        loc: usize,
    },
    Appl(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Copy, Clone)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Then,
    Cons,
    Greater,
    Less,
    Eq,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Add => "add",
                Op::Subtract => "subtract",
                Op::Multiply => "multiply",
                Op::Divide => "divide",
                Op::Modulo => "modulo",
                Op::Then => "`then`",
                Op::Cons => "cons",
                Op::Greater => "compare greater",
                Op::Less => "compare less",
                Op::Eq => "compare equal",
            }
        )
    }
}

#[derive(Debug, Clone)]
pub enum SyntaxError {
    Expected {
        expected: Token,
        found: Lexeme,
        loc: usize,
    },
    Unexpected(Lexeme, usize),
    CouldntFindInclude(PathBuf, usize),
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            lexer: Lexer::new(src),
        }
    }

    fn expect(&mut self, tk: Token) -> Result<(usize, Lexeme), SyntaxError> {
        let (loc, lexeme) = self.lexer.token();
        if lexeme.token == tk {
            Ok((loc, lexeme))
        } else {
            Err(SyntaxError::Expected {
                expected: tk,
                found: lexeme,
                loc,
            })
        }
    }

    pub fn program(&mut self) -> Result<Program, SyntaxError> {
        let mut defs = Vec::new();
        let mut includes = Vec::new();
        while self.lexer.peek_token().1.token != Token::Eof {
            if let Token::Include = self.lexer.peek_token().1.token {
                self.lexer.token();
                let (loc, string) = self.expect(Token::String)?;
                let filename = &string.lexeme.unwrap()[1..];
                includes.push((filename.into(), loc));
            } else {
                defs.push(self.def()?);
            }
        }
        Ok(Program { defs, includes })
    }

    fn def(&mut self) -> Result<Def, SyntaxError> {
        let (_, ident) = self.expect(Token::Word)?;
        let name = ident.lexeme.unwrap();
        self.expect(Token::Colon)?;
        let mut branches = Vec::new();
        branches.push(self.branch()?);
        while self.lexer.peek_token().1.token != Token::Semicolon {
            branches.push(self.branch()?);
        }
        self.expect(Token::Semicolon)?;
        Ok(Def { name, branches })
    }

    fn branch(&mut self) -> Result<(Pattern, Expr), SyntaxError> {
        let pattern = self.pattern()?;
        self.expect(Token::Arrow)?;
        let expr = self.expr()?;
        self.expect(Token::Semicolon)?;
        Ok((pattern, expr))
    }

    fn pattern(&mut self) -> Result<Pattern, SyntaxError> {
        let pat = self.cons_pattern()?;
        if let Token::Colon = self.lexer.peek_token().1.token {
            self.lexer.token();
            let guard = self.expr()?;
            Ok(Pattern {
                src: pat,
                guard: Some(guard),
            })
        } else {
            Ok(Pattern {
                src: pat,
                guard: None,
            })
        }
    }

    fn cons_pattern(&mut self) -> Result<PatternSource, SyntaxError> {
        let lhs = self.primary_pattern()?;
        Ok(if let Token::Comma = self.lexer.peek_token().1.token {
            self.lexer.token();
            PatternSource::Cons(Box::new(lhs), Box::new(self.cons_pattern()?))
        } else {
            lhs
        })
    }

    fn primary_pattern(&mut self) -> Result<PatternSource, SyntaxError> {
        let (loc, tk) = self.lexer.token();
        Ok(match tk.token {
            Token::Char => PatternSource::Char(tk.lexeme.unwrap().chars().next().unwrap()),
            Token::Nil => PatternSource::Nil,
            Token::True => PatternSource::Bool(true),
            Token::False => PatternSource::Bool(false),
            Token::Number => PatternSource::Number(tk.lexeme.unwrap().parse().unwrap()),
            Token::Wildcard => PatternSource::Wildcard,
            Token::Word => PatternSource::Named(tk.lexeme.unwrap()),
            Token::LeftParen => {
                let pattern = self.cons_pattern()?;
                self.expect(Token::RightParen)?;
                pattern
            }
            _ => return Err(SyntaxError::Unexpected(tk, loc)),
        })
    }

    fn expr(&mut self) -> Result<Expr, SyntaxError> {
        self.then_expr()
    }

    fn call(&mut self) -> Result<Expr, SyntaxError> {
        let mut func = self.primary()?;
        while let Token::LeftParen = self.lexer.peek_token().1.token {
            self.lexer.token();
            let arg = self.expr()?;
            self.expect(Token::RightParen)?;
            func = Expr::Appl(Box::new(func), Box::new(arg));
        }
        Ok(func)
    }

    fn then_expr(&mut self) -> Result<Expr, SyntaxError> {
        let mut lhs = self.cons_expr()?;
        while let Token::Then = self.lexer.peek_token().1.token {
            let (loc, _) = self.lexer.token();
            let rhs = self.cons_expr()?;
            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: Op::Then,
                loc,
            };
        }
        Ok(lhs)
    }

    fn cons_expr(&mut self) -> Result<Expr, SyntaxError> {
        let lhs = self.compare_expr()?;
        Ok(if let Token::Comma = self.lexer.peek_token().1.token {
            let (loc, _) = self.lexer.token();
            Expr::Binary {
                lhs: Box::new(lhs),
                rhs: Box::new(self.cons_expr()?),
                op: Op::Cons,
                loc,
            }
        } else {
            lhs
        })
    }

    fn compare_expr(&mut self) -> Result<Expr, SyntaxError> {
        let mut lhs = self.additive_expr()?;
        while let Token::Greater | Token::Less | Token::Eq = self.lexer.peek_token().1.token {
            let (loc, op) = self.lexer.token();
            let op = match op.token {
                Token::Greater => Op::Greater,
                Token::Less => Op::Less,
                Token::Eq => Op::Eq,
                _ => unreachable!(),
            };
            let rhs = self.additive_expr()?;
            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op,
                loc,
            };
        }

        Ok(lhs)
    }

    fn additive_expr(&mut self) -> Result<Expr, SyntaxError> {
        let mut lhs = self.term()?;
        while let Token::Plus | Token::Minus = self.lexer.peek_token().1.token {
            let (loc, op) = self.lexer.token();
            let op = match op.token {
                Token::Plus => Op::Add,
                Token::Minus => Op::Subtract,
                _ => unreachable!(),
            };
            let rhs = self.term()?;
            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op,
                loc,
            };
        }

        Ok(lhs)
    }

    fn term(&mut self) -> Result<Expr, SyntaxError> {
        let mut lhs = self.call()?;
        while let Token::Star | Token::Slash | Token::Percent = self.lexer.peek_token().1.token {
            let (loc, op) = self.lexer.token();
            let op = match op.token {
                Token::Star => Op::Multiply,
                Token::Slash => Op::Divide,
                Token::Percent => Op::Modulo,
                _ => unreachable!(),
            };
            let rhs = self.call()?;
            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op,
                loc,
            };
        }

        Ok(lhs)
    }

    fn primary(&mut self) -> Result<Expr, SyntaxError> {
        let (loc, tk) = self.lexer.token();
        Ok(match tk.token {
            Token::True => Expr::Bool(true, loc),
            Token::False => Expr::Bool(false, loc),
            Token::Char => Expr::Char(tk.lexeme.unwrap().chars().next().unwrap(), loc),
            Token::String => {
                let lexeme = tk.lexeme.unwrap();
                Expr::String(lexeme[1..lexeme.len()].to_string(), loc)
            }
            Token::Number => Expr::Number(tk.lexeme.unwrap().parse().unwrap(), loc),
            Token::Nil => Expr::Nil(loc),
            Token::Word => Expr::Named(tk.lexeme.unwrap(), loc),
            Token::LeftParen => {
                let expr = self.expr()?;
                self.expect(Token::RightParen)?;
                expr
            }
            _ => return Err(SyntaxError::Unexpected(tk, loc)),
        })
    }
}
