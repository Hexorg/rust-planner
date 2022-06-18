use super::Error;
use super::tokens::{Token, TokenData, Literal};
use std::fmt;

pub trait ExpressionVisitor<T, E> {
    fn visit_binary_expr(&mut self, token: &Token, left: &Expr, right: &Expr) -> Result<T, E>;
    fn visit_grouping_expr(&mut self, token: &Token, group: &Expr) -> Result<T, E>;
    fn visit_literal_expr(&mut self, token: &Token, data:&Literal) -> Result<T, E>;
    fn visit_variable_expr(&mut self, var_path:&[Token]) -> Result<T, E>;
    fn visit_unary_expr(&mut self, token: &Token, right: &Expr) -> Result<T, E>;
    fn visit_assignment_expr(&mut self, var_path:&[Token], left:&Expr) -> Result<T, E>;
    fn visit_call_expr(&mut self, token: &Token, name:&str, args:&[Expr]) -> Result<T, E>;
    fn visit_nop_expr(&mut self, token: &Token) -> Result<T, E>;
}

pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>), // left Token right
    Grouping(Box<Expr>, Token), // e.g. '(' expression ')'
    Literal(Token),
    Variable(Vec<Token>),
    Unary(Token, Box<Expr>), // Token right
    Assignment(Vec<Token>, Box<Expr>), // name, value
    Call(Token, Vec<Expr>), // callee, args
    Noop(Token)
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binary(left, op, right) => write!(f, "{} {} {}", left, op, right),
            Self::Grouping(g, _) => write!(f, "({})", g),
            Self::Literal(tok) => write!(f, "{}", tok.t),
            Self::Variable(var) =>  {
                let mut it = var.iter();
                it.by_ref().take(1).try_for_each(|v| write!(f, "{}", v))?;
                it.try_for_each(|v| write!(f, ".{}", v))
            },
            Self::Unary(op, right) => write!(f, "{}{}", op, right),
            Self::Assignment(var, right) =>  {
                let mut it = var.iter();
                it.by_ref().take(1).try_for_each(|v| write!(f, "{}", v))?;
                it.try_for_each(|v| write!(f, ".{}", v))?;
                write!(f, " = {}", right)
            },
            Self::Call(func, args) => {
                write!(f, "{}(", func)?; 
                let mut i = args.iter();
                i.by_ref().take(1).try_for_each(|expr| write!(f, "{}", expr))?;
                i.try_for_each(|expr| write!(f, ", {}", expr))?; 
                write!(f, ")")
            },
            Self::Noop(_) => write!(f, "nop"),
        }
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binary(left, op, right) => write!(f, "({:?}){}({:?})", left, op, right),
            Self::Grouping(g, _) => write!(f, "({:?})", g),
            Self::Literal(tok) => write!(f, ">{:?}<", tok.t),
            Self::Variable(var) => {
                let mut it = var.iter();
                it.by_ref().take(1).try_for_each(|v| write!(f, "var_{}", v))?;
                it.try_for_each(|v| write!(f, ".{}", v))
            },
            Self::Unary(op, right) => write!(f, "{}({:?})", op, right),
            Self::Assignment(var, right) => {
                let mut it = var.iter();
                it.by_ref().take(1).try_for_each(|v| write!(f, "var_{}", v))?;
                it.try_for_each(|v| write!(f, ".{}", v))?;
                write!(f, " = {:?}", right)
            },
            Self::Call(func, args) => {
                write!(f, "{}(", func)?; 
                let mut i = args.iter();
                i.by_ref().take(1).try_for_each(|expr| write!(f, "{:?}", expr))?;
                i.try_for_each(|expr| write!(f, ", {:?}", expr))?; 
                write!(f, ")")
            },
            Self::Noop(_) => write!(f, "nop"),
        }
    }
}


impl Expr {
    pub fn accept<R, E, T:ExpressionVisitor<R, E>>(&self, visitor: &mut T) -> Result<R, E> {
        match self {
            Expr::Binary(left, token, right) => visitor.visit_binary_expr(token, left, right),
            Expr::Grouping(g, token) => visitor.visit_grouping_expr(token, g),
            Expr::Literal(token @ Token{t:TokenData::Literal(l),..}) => visitor.visit_literal_expr(token, l),
            Expr::Variable(var_path) => visitor.visit_variable_expr(var_path),
            Expr::Unary(token, left) => visitor.visit_unary_expr(token, left),
            Expr::Assignment(var_path, left) => visitor.visit_assignment_expr(var_path, left),
            Expr::Call(token @ Token{t:TokenData::Label(s),..}, args) => visitor.visit_call_expr(token, s, args),
            Expr::Noop(token) => visitor.visit_nop_expr(token),
            _ => panic!("Unexpected expression structure. Mut be a bug in code by this point.")
        }
    }

    pub fn to_err(&self, msg:&str) -> Error {
        match self {
            Self::Binary(_, tok, _) |
            Self::Grouping(_, tok) |
            Self::Literal(tok) |
            Self::Unary(tok, _) |
            Self::Noop(tok) |
            Self::Call(tok, _) => tok.to_err(msg),
            Self::Assignment(tok, _) |
            Self::Variable(tok) => tok.get(0).unwrap().to_err(msg)
        }
    }

    pub fn line_no(&self) -> usize {
        match self {
            Self::Binary(_, tok, _) |
            Self::Grouping(_, tok) |
            Self::Literal(tok) |
            Self::Unary(tok, _) |
            Self::Noop(tok) |
            Self::Call(tok, _) => tok.line,
            Self::Assignment(tok, _) |
            Self::Variable(tok) => tok.get(0).unwrap().line
        }
    }
}