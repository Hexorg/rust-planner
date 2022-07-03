use super::Error;
use super::tokens::Token;
use std::fmt;

pub trait ExpressionVisitor<'a, T, E> {
    fn visit_binary_expr(&mut self, token: &Token<'a>, left: &Expr<'a>, right: &Expr<'a>) -> Result<T, E>;
    fn visit_grouping_expr(&mut self, token: &Token<'a>, group: &Expr<'a>) -> Result<T, E>;
    fn visit_literal_expr(&mut self, token: &Token<'a>) -> Result<T, E>;
    fn visit_variable_expr(&mut self, var_path:&[Token<'a>]) -> Result<T, E>;
    fn visit_unary_expr(&mut self, token: &Token<'a>, right: &Expr<'a>) -> Result<T, E>;
    fn visit_assignment_expr(&mut self, var_path:&[Token<'a>], left:&Expr<'a>) -> Result<T, E>;
    fn visit_call_expr(&mut self, target: &Token<'a>, args:&[Expr<'a>]) -> Result<T, E>;
    fn visit_nop_expr(&mut self, token: &Token<'a>) -> Result<T, E>;
}

#[derive(PartialEq)]
pub enum Expr<'a> {
    Binary(Box<Expr<'a>>, Token<'a>, Box<Expr<'a>>), // left Token right
    Grouping(Box<Expr<'a>>, Token<'a>), // e.g. '(' expression ')'
    Literal(Token<'a>),
    Variable(Vec<Token<'a>>),
    Unary(Token<'a>, Box<Expr<'a>>), // Token right
    Assignment(Vec<Token<'a>>, Box<Expr<'a>>), // name, value
    Call(Token<'a>, Vec<Expr<'a>>), // callee, args
    Nop(Token<'a>)
}

impl std::fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binary(left, op, right) => write!(f, "{} {} {}", left, op, right),
            Self::Grouping(g, _) => write!(f, "({})", g),
            Self::Literal(tok) => write!(f, "{}", tok),
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
                // let mut it = func.iter();
                // it.by_ref().take(1).try_for_each(|v| write!(f, "{}", v))?;
                // it.try_for_each(|v| write!(f, ".{}", v))?;
                write!(f, "{}(", func)?;
                // write!(f, "(")?;
                let mut it = args.iter();
                it.by_ref().take(1).try_for_each(|expr| write!(f, "{}", expr))?;
                it.try_for_each(|expr| write!(f, ", {}", expr))?; 
                write!(f, ")")
            },
            Self::Nop(_) => write!(f, "nop"),
        }
    }
}

impl std::fmt::Debug for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binary(left, op, right) => write!(f, "({:?}){}({:?})", left, op, right),
            Self::Grouping(g, _) => write!(f, "({:?})", g),
            Self::Literal(tok) => write!(f, ">{:?}<", tok),
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
                // let mut it = func.iter();
                // it.by_ref().take(1).try_for_each(|v| write!(f, "func_{}", v))?;
                // it.try_for_each(|v| write!(f, ".{}", v))?;
                // write!(f, "(")?;
                write!(f, "{}(", func)?;
                let mut it = args.iter();
                it.by_ref().take(1).try_for_each(|expr| write!(f, "{:?}", expr))?;
                it.try_for_each(|expr| write!(f, ", {:?}", expr))?; 
                write!(f, ")")
            },
            Self::Nop(_) => write!(f, "nop"),
        }
    }
}


impl<'a> Expr<'a> {
    pub fn accept<R, E, T:ExpressionVisitor<'a, R, E>>(&self, visitor: &mut T) -> Result<R, E> {
        match self {
            Expr::Binary(left, token, right) => visitor.visit_binary_expr(token, left, right),
            Expr::Grouping(g, token) => visitor.visit_grouping_expr(token, g),
            Expr::Literal(token) => visitor.visit_literal_expr(token),
            Expr::Variable(var_path) => visitor.visit_variable_expr(var_path),
            Expr::Unary(token, left) => visitor.visit_unary_expr(token, left),
            Expr::Assignment(var_path, left) => visitor.visit_assignment_expr(var_path, left),
            Expr::Call(token, args) => visitor.visit_call_expr(token, args),
            Expr::Nop(token) => visitor.visit_nop_expr(token),
        }
    }

    pub fn to_err(&self, msg:&str) -> Error {
        match self {
            Self::Binary(_, tok, _) |
            Self::Grouping(_, tok) |
            Self::Literal(tok) |
            Self::Unary(tok, _) |
            Self::Nop(tok)  |
            Self::Call(tok, _) => tok.to_err(msg),
            Self::Assignment(tok, _) |
            Self::Variable(tok) => tok.get(0).unwrap().to_err(msg)
        }
    }
}