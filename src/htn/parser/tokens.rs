use super::Error;
use std::{fmt, ops::Deref};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal<'a> {
    I(i32),
    F(f32),
    B(bool),
    S(&'a str)
}

impl fmt::Display for Literal<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Literal::*;
        match self {
            I(i) => write!(f, "{}", i),
            F(fv) => write!(f, "{}", fv),
            B(b) => write!(f, "{}", if *b {"true"} else {"false"}),
            S(s) => write!(f, "\"{}\"", s)
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenData<'a> {
    Task,
    Method,
    Else,
    Effects,
    Planning,
    Cost,
    Pass,
    For,
    As,
    Dot,
    Type,
    Identifier(&'a str),
    Literal(Literal<'a>),
    Include,
    Comma,
    Equals,
    EqualsEquals,
    Minus,
    Plus,
    Slash,
    Star,
    Greater,
    Smaller,
    GreaterOrEquals,
    SmallerOrEquals,
    NotEquals,
    SubtractFrom,
    AddTo,
    MultiplyBy,
    DivideBy,
    Or,
    And,
    Not,
    OpenParenthesis,
    CloseParenthesis,
    Colon,
    BlockStart,
    BlockEnd,
    StatementEnd,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Token<'a> {
    pub line: usize,
    pub col: usize,
    pub len: usize,
    pub t: TokenData<'a>
}

impl fmt::Display for TokenData<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenData::*;
        match self {
            Task => write!(f, "TASK"),
            Method => write!(f, "METHOD"),
            Else => write!(f, "ELSE"),
            Effects => write!(f, "EFFECTS"),
            Pass => write!(f, "PASS"),
            Cost => write!(f, "COST"),
            Include => write!(f, "INCLUDE"),
            Planning => write!(f, "PLANNING"),
            For => write!(f, "FOR"),
            As => write!(f, "AS"),
            Type => write!(f, "TYPE"),
            Dot => write!(f, "."),
            Identifier(label) => write!(f, "{}", label),
            Literal(literal) => write!(f, "{}", literal),
            Comma => write!(f, ","),
            Equals => write!(f, "="),
            EqualsEquals => write!(f, "=="),
            Minus => write!(f, "-"),
            Plus => write!(f, "+"),
            Slash => write!(f, "/"),
            Star => write!(f, "*"),
            Greater => write!(f, ">"),
            Smaller => write!(f, "<"),
            GreaterOrEquals => write!(f, ">="),
            SmallerOrEquals => write!(f, "<="),
            NotEquals => write!(f, "!="),
            SubtractFrom => write!(f, "-="),
            AddTo => write!(f, "+="),
            MultiplyBy => write!(f, "*="),
            DivideBy => write!(f, "/="),
            Or => write!(f, "|"),
            And => write!(f, "&"),
            Not => write!(f, "!"),
            OpenParenthesis => write!(f, "("),
            CloseParenthesis => write!(f, ")"),
            Colon => write!(f, ":"),
            BlockStart => write!(f, "{{"),
            BlockEnd => write!(f, "}}"),
            StatementEnd => write!(f, ";"),
        }
    }
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.t.fmt(f)
    }
}

impl Token<'_> {
    pub fn to_err(&self, msg:&str) -> Error {
        Error{line:self.line, col:self.col, message: String::from(msg) }
    }

    pub fn unwrap_identifier(&self) -> &str {
        match self.t {
            TokenData::Identifier(s) => s,
            _ => panic!("Expected identifier.")
        }
    }

    pub fn unwrap_literal(&self) -> Literal {
        match self.t {
            TokenData::Literal(l) => l,
            _ => panic!("Expected literal.")
        }
    }

}