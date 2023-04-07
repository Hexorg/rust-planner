use super::{Error, expression::{Identifier, Expr}, tokens::Span};


#[derive(Debug, PartialEq)]
pub struct Stmt<'a> {
    pub span: Span,
    pub kind: StmtKind<'a>,
}


#[derive(Debug, PartialEq)]
pub enum Block<'a> {
    Sorted(Vec<Stmt<'a>>),
    Unsorted(Vec<Stmt<'a>>),
}

impl<'a> Block<'a> {
    pub fn vec(&self) -> &Vec<Stmt<'a>> {
        match self {
            Self::Sorted(v) |
            Self::Unsorted(v) => v
        }
    }
    pub fn vec_mut(&mut self) -> &mut Vec<Stmt<'a>> {
        match self {
            Self::Sorted(v) |
            Self::Unsorted(v) => v
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum StmtKind<'a> {
    Task{
        name: Identifier<'a>,
        args: Vec<Expr<'a>>,
        body: Box<Stmt<'a>>,
    },
    Method{
        name: Identifier<'a>,
        args: Vec<Expr<'a>>,
        body: Box<Stmt<'a>>,
    },
    Axiom{
        name: Identifier<'a>,
        args: Vec<Expr<'a>>,
        body: Box<Stmt<'a>>,
    },
    Operator{
        name: Identifier<'a>,
        args: Vec<Expr<'a>>,
        body: Box<Stmt<'a>>, 
        cost: Expr<'a>,
    }, 
    ConditionalBlock(Box<Stmt<'a>>, Box<Stmt<'a>>),
    Block(Block<'a>),
    Expression(Expr<'a>),
    Include(&'a str),
}

