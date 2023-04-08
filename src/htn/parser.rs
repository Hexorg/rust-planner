// pub mod plan_rs;
pub mod pddl;
pub mod tokens;

use std::fmt;

use tokens::Span;

#[derive(PartialEq, Copy, Clone)]
pub enum Position {
    Span(Span),
    EOF
}

#[derive(PartialEq, Clone)]
pub struct Error {
    pub pos: Position,
    pub message: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.pos {
            Position::Span(s) => write!(f, "line:{} col:{} {}", s.line, s.col, self.message),
            Position::EOF => write!(f, "{} at the end of file.", self.message)
        }
        
    }
}
impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.pos {
            Position::Span(s) => write!(f, "line:{} col:{} len:{} {}", s.line, s.col, s.len, self.message),
            Position::EOF => write!(f, "{} at the end of file.", self.message)
        }
    }
}
impl std::error::Error for Error { }

impl Error {
    pub fn new(pos: Position, msg:&str) -> Self {
        Self{pos, message:String::from(msg)}
    }
}