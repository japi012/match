use std::{fmt, str::CharIndices};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    Word,
    Colon,
    Semicolon,
    LeftParen,
    RightParen,
    Arrow,
    Wildcard,

    Char,
    String,
    Number,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Comma,

    Greater,
    Less,
    Eq,

    Then,
    Nil,
    True,
    False,

    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::Word => "word",
                Token::Colon => "`:`",
                Token::Semicolon => "`;`",
                Token::LeftParen => "`(`",
                Token::RightParen => "`)`",
                Token::Arrow => "`->`",
                Token::Wildcard => "`_`",

                Token::Char => "character",
                Token::String => "string",
                Token::Number => "number",

                Token::Plus => "`+`",
                Token::Minus => "`-`",
                Token::Star => "`*`",
                Token::Slash => "`/`",
                Token::Percent => "`%`",
                Token::Comma => "`,`",

                Token::Greater => "`>`",
                Token::Less => "`<`",
                Token::Eq => "`=`",

                Token::Then => "keyword `then`",
                Token::Nil => "keyword `nil`",
                Token::True => "keyword `true`",
                Token::False => "keyword `false`",

                Token::Eof => "end of file",
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Lexeme {
    pub token: Token,
    pub lexeme: Option<String>,
}

impl Lexeme {
    fn new(token: Token, lexeme: Option<String>) -> Self {
        Self { token, lexeme }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    src: &'a str,
    chars: CharIndices<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            chars: src.char_indices(),
        }
    }

    fn peek(&self) -> Option<(usize, char)> {
        let mut chars = self.chars.clone();
        chars.next()
    }

    pub fn peek_token(&self) -> (usize, Lexeme) {
        let mut cloned = self.clone();
        cloned.token()
    }

    fn skip_whitespace(&mut self) {
        while self.peek().is_some_and(|(_, c)| c.is_whitespace()) {
            self.chars.next();
        }
    }

    pub fn token(&mut self) -> (usize, Lexeme) {
        self.skip_whitespace();

        let Some((index, c)) = self.chars.next() else {
            return (self.src.len(), Lexeme::new(Token::Eof, None));
        };

        match c {
            '(' => (index, Lexeme::new(Token::LeftParen, None)),
            ')' => (index, Lexeme::new(Token::RightParen, None)),
            ':' => (index, Lexeme::new(Token::Colon, None)),
            ';' => (index, Lexeme::new(Token::Semicolon, None)),
            ',' => (index, Lexeme::new(Token::Comma, None)),
            '<' => (index, Lexeme::new(Token::Greater, None)),
            '>' => (index, Lexeme::new(Token::Less, None)),
            '=' => (index, Lexeme::new(Token::Eq, None)),
            '-' => {
                if let Some((_, '>')) = self.peek() {
                    self.chars.next();
                    (index, Lexeme::new(Token::Arrow, None))
                } else {
                    (index, Lexeme::new(Token::Minus, None))
                }
            }
            '+' => (index, Lexeme::new(Token::Plus, None)),
            '*' => (index, Lexeme::new(Token::Star, None)),
            '/' => (index, Lexeme::new(Token::Slash, None)),
            '%' => (index, Lexeme::new(Token::Percent, None)),
            '"' => {
                let mut last = index + 1;
                while self.peek().is_some_and(|(_, c)| c != '"') {
                    self.chars.next();
                    last += 1;
                }

                self.chars.next();

                (
                    index,
                    Lexeme::new(Token::String, Some(self.src[index..last].to_string())),
                )
            }
            '\'' => {
                let char = match self.chars.next() {
                    Some((_, char)) => char,
                    None => '\0',
                };
                self.chars.next();

                (index, Lexeme::new(Token::Char, Some(char.to_string())))
            }
            c if c.is_ascii_digit() => {
                let mut last = index + 1;
                let mut is_float = false;
                while self
                    .peek()
                    .is_some_and(|(_, c)| c.is_ascii_digit() || c == '.')
                {
                    if self.peek().unwrap().1 == '.' && is_float {
                        break;
                    }
                    let (_, c) = self.chars.next().unwrap();
                    if c == '.' {
                        is_float = true;
                    }
                    last += 1;
                }
                (
                    index,
                    Lexeme::new(Token::Number, Some(self.src[index..last].to_string())),
                )
            }

            _ => {
                let mut last = index + 1;
                const SPECIAL: &str = "+->*/():,;";
                while self
                    .peek()
                    .is_some_and(|(_, c)| !c.is_whitespace() && !SPECIAL.contains(c))
                {
                    self.chars.next();
                    last += 1;
                }

                let word = &self.src[index..last];

                (
                    index,
                    match word {
                        "then" => Lexeme::new(Token::Then, None),
                        "true" => Lexeme::new(Token::True, None),
                        "false" => Lexeme::new(Token::False, None),
                        "nil" => Lexeme::new(Token::Nil, None),
                        "_" => Lexeme::new(Token::Wildcard, None),
                        _ => Lexeme::new(Token::Word, Some(self.src[index..last].to_string())),
                    },
                )
            }
        }
    }
}
