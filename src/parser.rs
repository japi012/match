use std::fmt;

use crate::lexer::{Lexeme, Lexer, Token};

#[derive(Debug, Clone)]
pub struct Def {
    pub name: String,
    pub branches: Vec<(Pattern, Expr)>,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Number(f64),
    Char(char),
    Named(String),
    Cons(Box<Pattern>, Box<Pattern>),
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

    pub fn program(&mut self) -> Result<Vec<Def>, SyntaxError> {
        let mut program = Vec::new();
        while self.lexer.peek_token().1.token != Token::Eof {
            program.push(self.def()?);
        }
        Ok(program)
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
        let lhs = self.primary_pattern()?;
        Ok(if let Token::Comma = self.lexer.peek_token().1.token {
            self.lexer.token();
            Pattern::Cons(Box::new(lhs), Box::new(self.pattern()?))
        } else {
            lhs
        })
    }

    fn primary_pattern(&mut self) -> Result<Pattern, SyntaxError> {
        let (loc, tk) = self.lexer.token();
        Ok(match tk.token {
            Token::Char => Pattern::Char(tk.lexeme.unwrap().chars().next().unwrap()),
            Token::Nil => Pattern::Nil,
            Token::True => Pattern::Bool(true),
            Token::False => Pattern::Bool(false),
            Token::Number => Pattern::Number(tk.lexeme.unwrap().parse().unwrap()),
            Token::Wildcard => Pattern::Wildcard,
            Token::Word => Pattern::Named(tk.lexeme.unwrap()),
            Token::LeftParen => {
                let pattern = self.pattern()?;
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
        while let Token::Star | Token::Slash = self.lexer.peek_token().1.token {
            let (loc, op) = self.lexer.token();
            let op = match op.token {
                Token::Star => Op::Multiply,
                Token::Slash => Op::Divide,
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
