use super::Error;
use std::fmt;

#[derive(Clone, Debug)]
pub enum Literal {
    I(i32),
    F(f32),
    B(bool),
    S(String)
}

impl fmt::Display for Literal {
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


#[derive(Debug, Clone)]
pub enum TokenData {
    Task,
    Method,
    Else,
    Effects,
    Cost,
    Pass,
    On,
    As,
    Dot,
    Type,
    Label(String),
    Literal(Literal),
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
    EOF
}

#[derive(Clone, Debug)]
pub struct Token {
    pub line: usize,
    pub col: usize,
    pub len: usize,
    pub t: TokenData,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.t)
    }
}

impl fmt::Display for TokenData {
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
            On => write!(f, "ON"),
            As => write!(f, "AS"),
            Type => write!(f, "TYPE"),
            Dot => write!(f, "."),
            Label(l) => write!(f, "{}", l),
            Literal(l) => write!(f, "'{}'", l),
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
            EOF => write!(f, "<<EOF"),
        }
    }
}

impl Token {
    pub fn to_err(&self, msg:&str) -> Error {
        Error { line: self.line, col: self.col, message: String::from(msg) }
    }
}