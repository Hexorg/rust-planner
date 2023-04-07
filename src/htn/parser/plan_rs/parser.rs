use std::{fmt, iter::Peekable, convert::{TryInto, TryFrom}};

pub mod plan_rs;

use plan_rs::tokens::{Token, Span, TokenKind::*, Literal::*};
use plan_rs::lexer::Lexer;
use plan_rs::expression::{Expr, ExprKind};
use plan_rs::statement::{Stmt, StmtKind, Block};

use crate::htn::parser::expression::BinaryOp;

use self::expression::UnaryOp;




pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Stmt<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if let None = self.lexer.peek() {
            None
        } else {
            let r = self.statement();
            if r.is_err() {
                self.error_recover();
            }
            Some(r)
        }
    }
}

impl<'a> Parser<'a> {
    fn error_recover(&mut self) {
        loop {
            if self.lexer.next_if(|t| match t {Ok(Token{kind:Task,..})=>false,_=>true}).is_none() {
                break
            }
        }
    }
    fn primary(&mut self) -> Result<Expr<'a>, Error> {
        // Literal | Label | "(" expression ")"
        let token = if let Some(t) = self.lexer.next() { t } else {
            Err(Error::new(Span::eof(), "Unexpected end of file."))
        };
        match token {
            Ok(Token{span, kind:Literal(l)}) => Ok(Expr{span, kind:ExprKind::Literal(l)}),
            Ok(Token{span, kind:Identifier(i)}) => {
                let mut span = span;
                let mut varpath = vec![i];
                while self.lexer.next_if(|t| match t { Ok(Token{kind:Dot,..}) => true, _=>false}).is_some() {
                    match self.lexer.next() {
                        Some(Ok(Token{span:tspan, kind:Identifier(i)})) => {
                            varpath.push(i);
                            span.len += 1 + tspan.len;
                            Ok(())
                        },
                        Some(Ok(token)) => Err(Error::new(token.span, "Expected identifier.")),
                        Some(Err(e)) => Err(e),
                        None => Err(Error::new(Span::eof(), "Unexpected end of file.")),
                    }?;
                }
                Ok(Expr{span, kind:ExprKind::Variable(varpath)})
            },
            Ok(Token{span, kind:OpenParenthesis}) => {
                let mut span = span;
                let expr = self.expression()?;
                if self.lexer.next_if(|t| match t { Ok(Token{kind:CloseParenthesis,..})=>true,_=>false}).is_some() {
                    span.len += expr.span.len + 1;
                    Ok(Expr{span, kind:ExprKind::Grouping(Box::new(expr))})
                } else {
                    span.col = expr.span.col + expr.span.len;
                    span.line = expr.span.line;
                    span.len = 1;
                    Err(Error::new(span, "Expected ')' after expression."))
                }
            },
            Err(e) => Err(e),
            Ok(token) => Err(Error::new(token.span, &format!("Unexpected token {:?}.", token)))
        }
    }

    fn call(&mut self) -> Result<Expr<'a>, Error> {
        let expr = self.primary()?;
        if let Some(Ok(Token{kind:OpenParenthesis,..})) = self.lexer.peek() {
            self.lexer.next();
            if let ExprKind::Variable(name) = expr.kind {
                let mut span = expr.span;
                let mut args = Vec::<Expr>::new();
                loop {
                    if self.lexer.next_if(|t| match t { Ok(Token{kind:CloseParenthesis,..})=>true,_=>false}).is_some() { 
                        span.len += 1;
                        break
                    }
                    let next_identifier = self.expression()?;
                    span.len += next_identifier.span.len + 1;
                    args.push(next_identifier);
                    match self.lexer.next() {
                        Some(Ok(Token{kind:Comma,..})) => (),
                        Some(Ok(Token{kind:CloseParenthesis,..})) => break,
                        Some(Ok(token)) => return Err(Error::new(token.span, "Expected ','.")),
                        Some(Err(e)) => return Err(e),
                        None => return Err(Error::new(Span::eof(), "Unexpected end of file.")),
                    }
                }
                Ok(Expr{span, kind:ExprKind::Call(name, args)})
            } else {
                Err(Error::new(expr.span, "Expected identifier."))
            }
        } else {
            Ok(expr)
        }
    }

    fn unary(&mut self) -> Result<Expr<'a>, Error> {
        if let Some(Ok(token)) = self.lexer.next_if(|t| match t { Ok(Token{kind:ExclamationPoint | Minus,..})=>true,_=>false}) {
            let mut span = token.span;
            let right = Box::new(self.unary()?);
            span.len += right.span.len;
            Ok(Expr{span, kind:ExprKind::Unary(token.try_into().unwrap(), right)})
        } else {
            self.call()
        }
    }
    fn factor(&mut self) -> Result<Expr<'a>, Error> {
        let mut expr = self.unary()?;
        while let Some(Ok(operator)) = self.lexer.next_if(|t| match t { Ok(Token{kind:Slash | Star,..})=>true,_=>false}) {
            let right = self.unary()?;
            let mut span = expr.span;
            span.len += right.span.len + 1;
            expr = Expr{span, kind:ExprKind::Binary(Box::new(expr), operator.try_into().unwrap(), Box::new(right))};
        }
        Ok(expr)
    }
    fn term(&mut self) -> Result<Expr<'a>, Error> {
        let mut expr = self.factor()?;
        while let Some(Ok(operator)) = self.lexer.next_if(|t| match t { Ok(Token{kind:Minus | Plus,..})=>true,_=>false}) {
            let right = self.factor()?;
            let mut span = expr.span;
            span.len += right.span.len + 1;
            expr = Expr{span, kind:ExprKind::Binary(Box::new(expr), operator.try_into().unwrap(), Box::new(right))};
        }
        Ok(expr)
    }
    fn comparison(&mut self) -> Result<Expr<'a>, Error> {
        let mut expr = self.term()?;
        while let Some(Ok(operator)) = self.lexer.next_if(|t| match t { Ok(Token{kind:Greater | GreaterOrEquals | Smaller | SmallerOrEquals,..})=>true,_=>false}) {
            let right = self.term()?;
            let mut span = expr.span;
            span.len += right.span.len + 1;
            expr = Expr{span, kind:ExprKind::Binary(Box::new(expr), operator.try_into().unwrap(), Box::new(right))};
        }
        Ok(expr)
    }
    fn equality(&mut self) -> Result<Expr<'a>, Error> {
        let mut expr =self.comparison()?;
        while let Some(Ok(operator)) = self.lexer.next_if(|t| match t { Ok(Token{kind:NotEquals | EqualsEquals,..})=>true,_=>false}) {
            let right = self.comparison()?;
            let mut span = expr.span;
            span.len += right.span.len + 1;
            expr = Expr{span, kind:ExprKind::Binary(Box::new(expr), operator.try_into().unwrap(), Box::new(right))};
        }
        Ok(expr)
    }
    fn logic(&mut self) -> Result<Expr<'a>, Error> {
        let mut expr = self.equality()?;
        while let Some(Ok(operator)) = self.lexer.next_if(|t| match t { Ok(Token{kind:Or | And,..})=>true,_=>false}) {
            let right = self.equality()?;
            let mut span = expr.span;
            span.len += right.span.len + 1;
            expr = Expr{span, kind:ExprKind::Binary(Box::new(expr), operator.try_into().unwrap(), Box::new(right))};
        }
        Ok(expr)
    }
    fn assignment(&mut self) -> Result<Expr<'a>, Error> {
        let target = self.logic()?;
        if let Some(Ok(operator)) = self.lexer.next_if(|t| match t { Ok(Token{kind:Equals | AddTo | SubtractFrom | DivideBy | MultiplyBy,..})=>true,_=>false}) {
            let right = self.expression()?;
            let mut span = target.span;
            span.len += right.span.len + 1;
            if let ExprKind::Variable(target) = target.kind {
                let value_expr = match operator {
                    Token{span, kind:AddTo,..} => Expr{span, kind:ExprKind::Binary(Box::new(Expr{span, kind:ExprKind::Variable(target.clone())}), BinaryOp::Add, Box::new(right))},
                    Token{span, kind:SubtractFrom,..} => Expr{span, kind:ExprKind::Binary(Box::new(Expr{span, kind:ExprKind::Variable(target.clone())}), BinaryOp::Sub, Box::new(right))},
                    Token{span, kind:MultiplyBy,..} => Expr{span, kind:ExprKind::Binary(Box::new(Expr{span, kind:ExprKind::Variable(target.clone())}), BinaryOp::Mul, Box::new(right))},
                    Token{span, kind:DivideBy,..} => Expr{span, kind:ExprKind::Binary(Box::new(Expr{span, kind:ExprKind::Variable(target.clone())}), BinaryOp::Div, Box::new(right))},
                    _ => right,
                };
                Ok(Expr{span, kind:ExprKind::Assignment(target, Box::new(value_expr))})
            } else {
                Err(Error::new(target.span, "Expected identifier."))
            }
        } else {
            Ok(target)
        }
    }
    
    fn expression(&mut self) -> Result<Expr<'a>, Error> {
        if let Some(Ok(token)) = self.lexer.next_if(|t| match t {Ok(Token{kind:Pass,..})=>true,_=>false}) {
            Ok(Expr{span:token.span, kind:ExprKind::Nop})
        } else {
            self.assignment()
        }
    }

    fn task_statement(&mut self) -> Result<Stmt<'a>, Error> {
        let _task = self.lexer.next();
        let nameAndArgs = self.call()?;
        if let ExprKind::Call(name, args) = nameAndArgs.kind {
            if let Some(Ok(_)) = self.lexer.next_if(|t| match t {Ok(Token{kind:Colon,..})=>true,_=>false}) {
                if let Some(Ok(_)) = self.lexer.next_if(|t| match t {Ok(Token{kind:StatementEnd,..})=>true,_=>false}) {
                    let body = self.statement()?;
                    Ok(Stmt{span:nameAndArgs.span, kind:StmtKind::Task { name, args, body:Box::new(body) }})
                } else {
                    Err(Error::new(nameAndArgs.span, "Expexted newline after ':'."))
                }
            } else {
                Err(Error::new(nameAndArgs.span, "Expexted ':' after task declaration."))
            }
        } else {
            Err(Error::new(nameAndArgs.span, "Expected task name and arguments."))
        }
    }
        
    fn expression_statement(&mut self) -> Result<Stmt<'a>, Error> {
        let expr = self.expression()?;
        let stmt = Stmt{span:expr.span, kind:StmtKind::Expression(expr)};
        match self.lexer.next() {
            Some(Ok(Token{kind:StatementEnd,..})) => Ok(stmt),
            Some(Ok(token)) => Err(Error::new(token.span, "Expected new line after statement.")),
            Some(Err(e)) => Err(e),
            None => Err(Error::new(Span::eof(), "Unexpected end of file.")),
        }
       
    }
    fn block_statement(&mut self) -> Result<Stmt<'a>, Error> {
        let blk = self.lexer.next().unwrap().unwrap();
        let mut stmts = if let Some(Ok(Token{kind:Sorted,..})) = self.lexer.peek() {
            Block::Sorted(Vec::new())
        } else {
            Block::Unsorted(Vec::new())
        };
        loop {
            stmts.vec_mut().push(self.statement()?);
            if self.lexer.next_if(|t| match t { Ok(Token{kind:BlockEnd,..}) => true, _ => false }).is_some() {
                break Ok(Stmt{span:blk.span, kind:StmtKind::Block(stmts)})
            } 
        }
        
    }
    fn operator_statement(&mut self) -> Result<Stmt<'a>, Error> {
        let _on = self.lexer.next();
        let nameAndArgs = self.call()?;
        if let ExprKind::Call(name, args) = nameAndArgs.kind {
            let cost = if let Some(Ok(token)) = self.lexer.next_if(|t| match t {Ok(Token{kind:Cost,..})=>true,_=>false}) {
                self.expression()?
            } else {
                Expr{span:nameAndArgs.span, kind:ExprKind::Literal(I(1))}
            };
            if let Some(Ok(token)) = self.lexer.next_if(|t| match t {Ok(Token{kind:Colon,..})=>true,_=>false}) {
                let body = self.statement()?;
                Ok(Stmt{span:nameAndArgs.span, kind:StmtKind::Operator{name, args, body:Box::new(body), cost}})
            } else {
                Err(Error::new(nameAndArgs.span, "Expexted ':' after task declaration."))
            }
        } else {
            Err(Error::new(nameAndArgs.span, "Expected task name and arguments."))
        }
    }
    fn axiom_statement(&mut self) -> Result<Stmt<'a>, Error> {
        let _def = self.lexer.next();
        let nameAndArgs = self.call()?;
        if let ExprKind::Call(name, args) = nameAndArgs.kind {
            if let Some(Ok(token)) = self.lexer.next_if(|t| match t {Ok(Token{kind:Colon,..})=>true,_=>false}) {
                let body = self.statement()?;
                Ok(Stmt{span:nameAndArgs.span, kind:StmtKind::Axiom { name, args, body:Box::new(body) }})
            } else {
                Err(Error::new(nameAndArgs.span, "Expexted ':' after task declaration."))
            }
        } else {
            Err(Error::new(nameAndArgs.span, "Expected task name and arguments."))
        }
    }
    fn conditional_block_statement(&mut self) -> Result<Stmt<'a>, Error> {
        let when = self.lexer.next().unwrap().unwrap();
        if let Some(Ok(token)) = self.lexer.next_if(|t| match t {Ok(Token{kind:Colon,..})=>true,_=>false}) {
            let preconditions = self.statement()?;
            if let Some(Ok(do_token)) = self.lexer.next_if(|t| match t {Ok(Token{kind:Do,..})=>true,_=>false}) {
                if let Some(Ok(token)) = self.lexer.next_if(|t| match t {Ok(Token{kind:Colon,..})=>true,_=>false}) {
                    let do_block = self.statement()?;
                    Ok(Stmt{span:when.span, kind:StmtKind::ConditionalBlock(Box::new(preconditions), Box::new(do_block))})
                } else {
                    Err(Error::new(do_token.span, "Expected ':' after do."))
                }
            } else {
                Err(Error::new(when.span, "Expected 'do' block after when block."))
            }
        } else {
            Err(Error::new(when.span, "Expected ':' afer when."))
        }

    }
    fn include_statement(&mut self) -> Result<Stmt<'a>, Error> {
        let inc = self.lexer.next().unwrap().unwrap();
        let target = match self.lexer.next() {
            Some(Ok(token @ Token{kind:Literal(S(_)),..})) => Ok(token),
            Some(Ok(token)) => Err(Error::new(token.span, "Expected string literal.")),
            Some(Err(e)) => Err(e),
            None => Err(Error::new(Span::eof(), "Unexpected end of file.")),
        }?;
        match self.lexer.next() {
            Some(Ok(Token{kind:StatementEnd,..})) => {
                let mut span = inc.span;
                span.len += target.span.len;
                Ok(Stmt{span, kind:StmtKind::Include(target.unwrap_literal().unwrap_str())})}
                ,
            Some(Ok(token)) => Err(Error::new(token.span, "Expected new line after statement.")),
            Some(Err(e)) => Err(e),
            None => Err(Error::new(Span::eof(), "Unexpected end of file.")),
        }
        
    }

    fn statement(&mut self)-> Result<Stmt<'a>, Error> {
        match self.lexer.peek() {
            Some(Ok(Token{kind:Task,..})) => self.task_statement(),
            Some(Ok(Token{kind:On,..})) => self.operator_statement(),
            Some(Ok(Token{kind:Def,..})) => self.axiom_statement(),
            Some(Ok(Token{kind:Include,..})) => self.include_statement(),
            Some(Ok(Token{kind:When,..})) => self.conditional_block_statement(),
            Some(Ok(Token{kind:BlockStart,..})) => self.block_statement(),
            Some(Err(_)) => Err(self.lexer.next().unwrap().unwrap_err()),
            None => Err(Error::new(Span::eof(), "Unexpected end of file.")),
            _ => self.expression_statement()
        }

    }

    pub fn new(code:&'a str) -> Self {
        let lexer = Lexer::<'a>::new(code).peekable();
        Self{lexer}
    }
}

#[cfg(test)]
mod tests {
    use crate::htn::parser::{Parser, Error, statement::{Stmt, StmtKind, Block}, expression::{Expr, ExprKind}, tokens::{Span, Literal::*}};

    #[test]
    fn test_include() {
        let code = "include \"src\"";
        use StmtKind::*;
        let mut parser = Parser::new(code);
        assert_eq!(parser.next(), Some(Ok(Stmt{span:Span{line:1, col:1, len:10, is_EOF:false}, kind:Include("src")})));
        assert_eq!(parser.next(), None);
    }
    #[test]
    fn test_task() {
        let code = "task transport(package):\n\tpass";
        let mut parser = Parser::new(code);
        // use StmtKind::*;
        assert_eq!(parser.next(), Some(Ok(Stmt{span:Span{line:1, col:6, len:17, is_EOF:false}, kind:StmtKind::Task{ 
            name:vec!["transport"], 
            args:vec![Expr{span:Span{line:1, col:16, len:7, is_EOF:false}, kind:ExprKind::Variable(vec!["package"])}], 
            body:Box::new(Stmt{span:Span{line:2, col:1, len:1, is_EOF:false}, kind:StmtKind::Block(
                Block::Sorted(vec![Stmt{span:Span{line:2, col:2, len:4, is_EOF:false}, kind:StmtKind::Expression(
                    Expr{span:Span{line:2, col:2, len:4, is_EOF:false}, kind:ExprKind::Nop}
                )}])
            )
             })}})));
        assert_eq!(parser.next(), None);
    }
}