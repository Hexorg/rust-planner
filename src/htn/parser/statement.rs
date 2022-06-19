use super::Error;
use super::tokens::{Token, TokenData, Literal};
use super::expression::Expr;
use std::fmt;


pub struct Binding {
    pub class_type: String,
    pub variable_name: String,
}

pub trait StatementVisitor<T, E> {
    fn visit_method(&mut self, token:&Token, name:&str, preconditions:Option<&Expr>, cost:Option<i32>, body:&Stmt, else_cost:Option<i32>, else_body:Option<&Stmt>) -> Result<T, E>;
    fn visit_task(&mut self, token:&Token, name:&str, binding:Option<&Binding>, preconditions:Option<&Expr>, cost:Option<i32>, body:&Stmt, effects:Option<&Stmt>) -> Result<T, E>;
    fn visit_block(&mut self, block:&[Stmt]) -> Result<T, E>;
    fn visit_expression(&mut self, expr:&Expr) -> Result<T, E>;
    fn visit_include(&mut self, token:&Token, filepath:&str) -> Result<T, E>;
    fn visit_type(&mut self, token:&Token, name:&str, body:&Stmt) -> Result<T, E>;
}

pub enum Stmt {
    Method{
        name:Token, 
        preconditions:Option<Expr>, 
        cost:Option<i32>, 
        body:Box<Stmt>, 
        else_cost:Option<i32>,
        else_body:Option<Box<Stmt>>,
    }, 
    Task{
        name:Token, 
        preconditions:Option<Expr>, 
        cost: Option<i32>,
        body:Box<Stmt>, 
        binding:Option<Binding>,
        effects:Option<Box<Stmt>>, 
    }, 
    Type{
        name: Token,
        body:Box<Stmt>,
    },
    Block(Vec<Stmt>),
    Expression(Expr),
    Include(Token),
}

pub struct StmtFormatter<'a, 'b> {
    depth: usize,
    f: &'a mut fmt::Formatter<'b>,
}

impl StmtFormatter<'_, '_> {
    const DEPTH_INCREASE:usize = 2;
}

impl<'a, 'b> From<&'a mut fmt::Formatter<'b>> for StmtFormatter<'a, 'b> {
    fn from(f: &'a mut fmt::Formatter<'b>) -> Self {
        StmtFormatter {depth: 0, f }
    }
}

impl StatementVisitor<(), std::fmt::Error> for StmtFormatter<'_, '_> {
    fn visit_method(&mut self, _:&Token, name:&str, preconditions:Option<&Expr>, cost:Option<i32>, body:&Stmt, else_cost:Option<i32>, else_body:Option<&Stmt>) -> std::fmt::Result {
        write!(self.f, "{:>depth$}method {}", "", name, depth=self.depth)?;
        preconditions.and_then(|p| Some(write!(self.f, "({})", p))).unwrap_or(Ok(()))?;
        cost.and_then(|c| Some(write!(self.f, " cost {}", c))).unwrap_or(Ok(()))?;
        write!(self.f, ":")?;
        body.accept(self)?;
        if let Some(else_body) = else_body {
            write!(self.f, "{:>depth$}else", "", depth=self.depth)?;
            else_cost.and_then(|c| Some(write!(self.f, " cost {}", c))).unwrap_or(Ok(()))?;
            write!(self.f, ":")?;
            else_body.accept(self)?;
        }
        Ok(())
    }

    fn visit_task(&mut self, _:&Token, name:&str, binding:Option<&Binding>, preconditions:Option<&Expr>, cost:Option<i32>, body:&Stmt, effects:Option<&Stmt>) -> std::fmt::Result {
        write!(self.f, "{:>depth$}task {}", "", name, depth=self.depth)?;
        preconditions.and_then(|p| Some(write!(self.f, "({})", p))).unwrap_or(Ok(()))?;
        binding.and_then(|Binding{class_type, variable_name}| Some(write!(self.f, " on {} as {}", class_type, variable_name))).unwrap_or(Ok(()))?;
        cost.and_then(|c| Some(write!(self.f, " cost {}", c))).unwrap_or(Ok(()))?;
        write!(self.f, ":");
        body.accept(self)?;
        if let Some(effects) = effects {
            write!(self.f, "{:>depth$}effects:", "", depth=self.depth)?;
            effects.accept(self)?;
        }
        Ok(())
    }

    fn visit_block(&mut self, block:&[Stmt]) -> std::fmt::Result{
        writeln!(self.f)?;
        self.depth += Self::DEPTH_INCREASE;
        block.iter().try_for_each(|stmt| stmt.accept(self))?;
        self.depth -= Self::DEPTH_INCREASE;
        Ok(())
    }

    fn visit_expression(&mut self, expr:&Expr) -> std::fmt::Result {
        writeln!(self.f, "{:>depth$}{}", "", expr, depth=self.depth)
    }

    fn visit_include(&mut self, _:&Token, filepath:&str) -> std::fmt::Result {
        write!(self.f, "{:>depth$}include \"{}\"", "", filepath, depth=self.depth)
    }

    fn visit_type(&mut self, _token:&Token, name:&str, body:&Stmt) -> std::fmt::Result {
        writeln!(self.f, "{:>depth$}type {}:", "", name, depth=self.depth)?;
        body.accept(self)

    }
}

impl Stmt {
    pub fn accept<R, E, T:StatementVisitor<R, E>>(&self, visitor:&mut T) -> Result<R, E> {
        match self {
            Stmt::Method { name:token @ Token{t:TokenData::Label(name),..}, preconditions, cost, body, else_cost, else_body } => visitor.visit_method(token, name, preconditions.as_ref(), *cost, body, *else_cost, else_body.as_deref()),
            Stmt::Task { name:token @ Token{t:TokenData::Label(name),..}, preconditions, cost, body, binding, effects } => visitor.visit_task(token, name, binding.as_ref(), preconditions.as_ref(), *cost, body, effects.as_deref()),
            Stmt::Block(v) => visitor.visit_block(v),
            Stmt::Expression(expr) => visitor.visit_expression(expr),
            Stmt::Type{name:token@Token{t:TokenData::Label(name),..}, body} => visitor.visit_type(token, name, body),
            Stmt::Include(token @ Token{t:TokenData::Literal(Literal::S(path)),..}) => visitor.visit_include(token, path),
            _ => panic!("Unexpected statement structure. Mut be a bug in code by this point.")
        }
    }
    pub fn to_err(&self, msg:&str) -> Error {
        match self {
            Stmt::Method{name,..} |
            Stmt::Task{name,..} |
            Stmt::Type{name,..} => name.to_err(msg),
            Stmt::Block(blk) => blk.first().expect("Unable to generate error for empty block.").to_err(msg),
            Stmt::Expression(e) => e.to_err(msg),
            Stmt::Include(e) => e.to_err(msg),
        }
    }
}

impl std::fmt::Display for Stmt {    
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut my_formatter = StmtFormatter::from(f);
        self.accept(&mut my_formatter)
    }
}
