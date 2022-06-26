use super::Error;
use super::tokens::{Token, TokenData, Literal};
use super::expression::Expr;
use std::fmt;

pub trait StatementVisitor<T, E> {
    fn visit_task_declaration(&mut self, name:&[Token]) -> Result<T, E>;
    fn visit_task(&mut self, name:&[Token], preconditions:Option<&Expr>, cost:Option<&Expr>, binding:Option<(&str, &str)>, body:&Stmt, effects:Option<&Stmt>, planning:Option<&Stmt>) -> Result<T, E>;
    fn visit_block(&mut self, block:&[Stmt]) -> Result<T, E>;
    fn visit_expression(&mut self, expr:&Expr) -> Result<T, E>;
    fn visit_include(&mut self, filepath:&Token) -> Result<T, E>;
    fn visit_type(&mut self, class:&Token, body:&Stmt) -> Result<T, E>;
}


#[derive(Debug, PartialEq)]
pub enum Stmt<'a> {
    TaskDeclaration{
        name:Vec<Token<'a>>,
    },
    Task{
        name:Vec<Token<'a>>, 
        preconditions:Option<Expr<'a>>, 
        cost: Option<Expr<'a>>,
        binding:Option<(&'a str, &'a str)>,
        body:Box<Stmt<'a>>, 
        effects:Option<Box<Stmt<'a>>>,
        planning:Option<Box<Stmt<'a>>>, 
    }, 
    Type{
        name: Token<'a>,
        body:Box<Stmt<'a>>,
    },
    Block(Vec<Stmt<'a>>),
    Expression(Expr<'a>),
    Include(Token<'a>),
}

pub struct StmtFormatter<'a, 'b> {
    depth: usize,
    f: &'a mut fmt::Formatter<'b>,
}

impl StmtFormatter<'_, '_> {
    const DEPTH_INCREASE:usize = 2;

    fn write_var_path(&mut self, var_path:&[Token]) -> std::fmt::Result {
        let mut it = var_path.iter();
        it.by_ref().take(1).try_for_each(|n| write!(self.f, "{}", n))?;
        it.try_for_each(|n| write!(self.f, ".{}", n))
    }
}

impl<'a, 'b> From<&'a mut fmt::Formatter<'b>> for StmtFormatter<'a, 'b> {
    fn from(f: &'a mut fmt::Formatter<'b>) -> Self {
        StmtFormatter {depth: 0, f }
    }
}

impl StatementVisitor<(), std::fmt::Error> for StmtFormatter<'_, '_> {
    fn visit_task_declaration(&mut self, name:&[Token]) -> std::fmt::Result {
        write!(self.f, "{:>depth$}task {}", "", depth=self.depth)?;
        self.write_var_path(name)
    }

    fn visit_task(&mut self, name:&[Token], preconditions:Option<&Expr>, cost:Option<&Expr>, binding:Option<(&str, &str)>, body:&Stmt, effects:Option<&Stmt>, planning:Option<&Stmt>) -> std::fmt::Result {
        self.visit_task_declaration(name)?;
        preconditions.and_then(|p| Some(write!(self.f, "({})", p))).unwrap_or(Ok(()))?;
        binding.and_then(|(class, variable)| Some(write!(self.f, " for {} as {}", class, variable))).unwrap_or(Ok(()))?;
        cost.and_then(|c| Some(write!(self.f, " cost {}", c))).unwrap_or(Ok(()))?;
        write!(self.f, ":")?;
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

    fn visit_include(&mut self, filepath:&Token) -> std::fmt::Result {
        write!(self.f, "{:>depth$}include \"{}\"", "", filepath, depth=self.depth)
    }

    fn visit_type(&mut self, class:&Token, body:&Stmt) -> std::fmt::Result {
        writeln!(self.f, "{:>depth$}type {}:", "", class, depth=self.depth)?;
        body.accept(self)

    }
}

impl Stmt<'_> {
    pub fn accept<R, E, T:StatementVisitor<R, E>>(&self, visitor:&mut T) -> Result<R, E> {
        match self {
            Stmt::Task {name, preconditions, cost, body, binding, effects, planning} => {
                visitor.visit_task(name, preconditions.as_ref(), cost.as_ref(), *binding, body, effects.as_deref(), planning.as_deref())
            },
            Stmt::Block(v) => visitor.visit_block(v),
            Stmt::Expression(expr) => visitor.visit_expression(expr),
            Stmt::Type{name, body} => visitor.visit_type(name, body),
            Stmt::Include(token) => visitor.visit_include(token),
            _ => panic!("Unexpected statement structure. Mut be a bug in code by this point.")
        }
    }
    pub fn to_err(&self, msg:&str) -> Error {
        match self {
            Stmt::TaskDeclaration{name} |
            Stmt::Task{name,..} => name[0].to_err(msg),
            Stmt::Type{name,..} => name.to_err(msg),
            Stmt::Block(blk) => blk.first().expect("Unable to generate error for empty block.").to_err(msg),
            Stmt::Expression(e) => e.to_err(msg),
            Stmt::Include(e) => e.to_err(msg),
        }
    }
}

impl std::fmt::Display for Stmt<'_> {    
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut my_formatter = StmtFormatter::from(f);
        self.accept(&mut my_formatter)
    }
}
