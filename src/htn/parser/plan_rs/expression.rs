use std::convert::TryFrom;

use super::tokens::{Span, Literal, Token, TokenKind};

#[derive(Debug, PartialEq)]
pub struct Expr<'a> {
    pub span: Span,
    pub kind: ExprKind<'a>
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
     Add,
     Sub,
     Mul,
     Div
}

impl TryFrom<Token<'_>> for BinaryOp {
    type Error = ();

    fn try_from(value: Token<'_>) -> Result<Self, Self::Error> {
        use TokenKind::*;
        match value.kind {
            Plus => Ok(Self::Add),
            Minus => Ok(Self::Sub),
            Star => Ok(Self::Mul),
            Slash => Ok(Self::Div),
            _ => Err(())
        }
    }
}


#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Not,
    Negate
}

impl TryFrom<Token<'_>> for UnaryOp {
    type Error = ();

    fn try_from(value: Token<'_>) -> Result<Self, Self::Error> {
        match value.kind {
            TokenKind::ExclamationPoint => Ok(Self::Not),
            TokenKind::Minus => Ok(Self::Negate),
            _ => Err(())
        }
    }
}

// #[derive(Debug, PartialEq)]
pub type Identifier<'a> = Vec<&'a str>;


#[derive(Debug, PartialEq)]
pub enum ExprKind<'a> {
    Binary(Box<Expr<'a>>, BinaryOp, Box<Expr<'a>>), // left Token right
    Grouping(Box<Expr<'a>>), // e.g. '(' expression ')'
    Literal(Literal<'a>),
    Variable(Identifier<'a>),
    Unary(UnaryOp, Box<Expr<'a>>), // Token right
    Assignment(Identifier<'a>, Box<Expr<'a>>), // name, value
    Call(Identifier<'a>, Vec<Expr<'a>>), // callee, args
    Nop
}
