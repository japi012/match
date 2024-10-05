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

#[derive(Debug, Clone)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    Then,
    Cons,
}
