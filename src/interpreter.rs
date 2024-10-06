use crate::parser::{Def, Expr, Op, Pattern};
use std::{collections::HashMap, fmt, rc::Rc};

#[derive(Clone)]
pub enum Value {
    Number(f64),
    Char(char),
    Bool(bool),
    Nil,
    Cons(Box<Value>, Box<Value>),

    Native(Rc<dyn Fn(Value) -> Result<Value, RuntimeError>>),
    Function(Def),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Char(c) => write!(f, "{c}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Nil => write!(f, "[nil]"),
            Value::Cons(head, tail) => {
                if let Value::Nil = **tail {
                    write!(f, "{head}")
                } else {
                    write!(f, "{head}{tail}")
                }
            }
            Value::Native(_) => write!(f, "[native fn]"),
            Value::Function(d) => write!(f, "[fn `{}`]", d.name),
        }
    }
}

#[derive(Debug, Clone)]
pub enum RuntimeError {
    UndefinedFunc(String, usize),
    CallNonFunc(Value, usize),
    DoesntSupportTypes(Value, Value, Op, usize),
    DivisionByZero(usize),
    UnmatchedValue(Value, usize),
    Explicit(usize),
}

type Builtins = HashMap<Box<str>, Rc<dyn Fn(Value) -> Result<Value, RuntimeError>>>;

#[derive(Clone)]
pub struct Interpreter {
    funcs: HashMap<Box<str>, Vec<(Pattern, Expr)>>,
    builtins: Builtins,
    locals: HashMap<Box<str>, Value>,
}

impl Default for Interpreter {
    fn default() -> Self {
        let mut builtins: Builtins = HashMap::new();
        builtins.insert(
            "println".into(),
            Rc::new(|v| {
                println!("{v}");
                Ok(Value::Nil)
            }),
        );
        builtins.insert(
            "print".into(),
            Rc::new(|v| {
                use std::io::Write;
                let mut stdout = std::io::stdout();
                print!("{v}");
                stdout.flush().unwrap();
                Ok(Value::Nil)
            }),
        );
        builtins.insert(
            "error".into(),
            Rc::new(|v| {
                eprintln!("{v}");
                Err(RuntimeError::Explicit(0))
            }),
        );
        builtins.insert(
            "type".into(),
            Rc::new(|v| {
                Ok(match v {
                    Value::Bool(_) => "bool",
                    Value::Char(_) => "char",
                    Value::Cons(_, _) => "cons",
                    Value::Function(_) | Value::Native(_) => "function",
                    Value::Nil => "nil",
                    Value::Number(_) => "number",
                }
                .chars()
                .rfold(Value::Nil, |acc, c| {
                    Value::Cons(Box::new(Value::Char(c)), Box::new(acc))
                }))
            }),
        );
        Self {
            funcs: HashMap::new(),
            builtins,
            locals: HashMap::new(),
        }
    }
}

impl Interpreter {
    pub fn register(&mut self, defs: Vec<Def>) {
        for def in defs {
            self.funcs.insert(def.name.into_boxed_str(), def.branches);
        }
    }

    fn eval_expr(&self, expr: Expr) -> Result<(Value, usize), RuntimeError> {
        match expr {
            Expr::Nil(pos) => Ok((Value::Nil, pos)),
            Expr::Number(number, pos) => Ok((Value::Number(number), pos)),
            Expr::Char(char, pos) => Ok((Value::Char(char), pos)),
            Expr::Bool(bool, pos) => Ok((Value::Bool(bool), pos)),
            Expr::String(s, pos) => Ok((
                s.chars().rfold(Value::Nil, |acc, c| {
                    Value::Cons(Box::new(Value::Char(c)), Box::new(acc))
                }),
                pos,
            )),
            Expr::Named(n, pos) => {
                let s = n.clone().into_boxed_str();
                if let Some(f) = self.builtins.get(&s) {
                    Ok((Value::Native(Rc::clone(f)), pos))
                } else if let Some(f) = self.funcs.get(&s) {
                    Ok((
                        Value::Function(Def {
                            branches: f.clone(),
                            name: n,
                        }),
                        pos,
                    ))
                } else if let Some(v) = self.locals.get(&s) {
                    Ok((v.clone(), pos))
                } else {
                    Err(RuntimeError::UndefinedFunc(n, pos))
                }
            }
            Expr::Appl(func, v) => match self.eval_expr(*func)? {
                (Value::Native(f), pos) => {
                    let (v, _) = self.eval_expr(*v)?;
                    f(v).map(|v| (v, pos)).map_err(|e| {
                        if let RuntimeError::Explicit(_) = e {
                            RuntimeError::Explicit(pos)
                        } else {
                            e
                        }
                    })
                }
                (Value::Function(f), pos) => {
                    let (v, loc) = self.eval_expr(*v)?;
                    let mut s = self.clone();
                    Ok((s.apply_func(f.branches, v, loc)?, pos))
                }
                (f, pos) => Err(RuntimeError::CallNonFunc(f, pos)),
            },
            Expr::Binary { lhs, rhs, op, loc } => {
                let (lhs, _) = self.eval_expr(*lhs)?;
                let (rhs, _) = self.eval_expr(*rhs)?;

                match (lhs, rhs, op) {
                    (Value::Number(lhs), Value::Number(rhs), Op::Add) => {
                        Ok((Value::Number(lhs + rhs), loc))
                    }
                    (Value::Number(lhs), Value::Number(rhs), Op::Subtract) => {
                        Ok((Value::Number(lhs - rhs), loc))
                    }
                    (Value::Number(lhs), Value::Number(rhs), Op::Multiply) => {
                        Ok((Value::Number(lhs * rhs), loc))
                    }
                    (Value::Number(lhs), Value::Number(rhs), Op::Divide) => {
                        if rhs == 0. {
                            Err(RuntimeError::DivisionByZero(loc))
                        } else {
                            Ok((Value::Number(lhs / rhs), loc))
                        }
                    }
                    (lhs, rhs, Op::Add | Op::Subtract | Op::Multiply | Op::Divide) => {
                        Err(RuntimeError::DoesntSupportTypes(lhs, rhs, op, loc))
                    }
                    (lhs, rhs, Op::Cons) => Ok((Value::Cons(Box::new(lhs), Box::new(rhs)), loc)),
                    (_, rhs, Op::Then) => Ok((rhs, loc)),
                }
            }
        }
    }

    pub fn run(&mut self, args: Vec<String>) -> Result<Option<Value>, RuntimeError> {
        if let Some(f) = self.funcs.get("main") {
            Ok(Some(self.apply_func(
                f.clone(),
                args.into_iter().rfold(Value::Nil, |acc, arg| {
                    Value::Cons(
                        Box::new(arg.chars().rfold(Value::Nil, |s, c| {
                            Value::Cons(Box::new(Value::Char(c)), Box::new(s))
                        })),
                        Box::new(acc),
                    )
                }),
                0,
            )?))
        } else {
            Ok(None)
        }
    }

    fn apply_func(
        &mut self,
        branches: Vec<(Pattern, Expr)>,
        value: Value,
        loc: usize,
    ) -> Result<Value, RuntimeError> {
        let saved = self.locals.clone();
        let mut matched = None;
        for (pat, expr) in branches {
            if self.apply_pattern(value.clone(), pat) {
                matched = Some(expr);
                break;
            }
        }
        if let Some(expr) = matched {
            let (value, _) = self.eval_expr(expr)?;
            self.locals = saved;
            Ok(value)
        } else {
            Err(RuntimeError::UnmatchedValue(value, loc))
        }
    }

    fn apply_pattern(&mut self, val: Value, pat: Pattern) -> bool {
        match (val, pat) {
            (v, Pattern::Named(n)) => {
                self.locals.insert(n.into(), v);
                true
            }
            (_, Pattern::Wildcard) => true,
            (Value::Nil, Pattern::Nil) => true,
            (Value::Number(n1), Pattern::Number(n2)) => n1 == n2,
            (Value::Bool(b1), Pattern::Bool(b2)) => b1 == b2,
            (Value::Char(c1), Pattern::Char(c2)) => c1 == c2,
            (Value::Cons(v1, v2), Pattern::Cons(p1, p2)) => {
                let p = self.apply_pattern(*v1, *p1);
                p && self.apply_pattern(*v2, *p2)
            }
            _ => false,
        }
    }
}