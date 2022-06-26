use std::{fmt, iter::Peekable};

pub mod tokens;
use tokens::{Token, TokenData::*, Literal::*};

pub mod lexer;
use lexer::Lexer;

pub mod expression;
use expression::Expr;

pub mod statement;
use statement::Stmt;


#[derive(PartialEq)]
pub struct Error {
    pub line: usize,
    pub col: usize,
    pub message: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line:{} col:{} {}", self.line, self.col, self.message)
    }
}
impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line:{} col:{} {}", self.line, self.col, self.message)
    }
}
impl std::error::Error for Error { }



pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Stmt<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if let None = self.lexer.peek() {
            None
        } else {
            Some(self.statement())
        }
    }
}

// macro_rules! punwrap {
//     ($s:expr) => {
//         match $s {
//             None => return Err(Error{line:0, col:0, message:String::from("Unexpected end of file.")}),
//             Some(Err(e)) => return Err(e),
//             Some(Ok(r)) => r,
//         }
//     }
// }


// macro_rules! pexpect {
//     ($s:expr, $($p:pat)|+, $do:block, $e:literal) => {
//         match $s.lexer.peek() {
//             $($p)|+ => {$s.lexer.next(); $do}
//             Some(Err(e)) => return Err(Error{line:e.line, col:e.col, message:String::from($e)}),
//             None => return Err(Error{line:0, col:0, message:String::from("Unexpected end of file.")+$e}),
//             Some(Ok(t)) => return Err(t.to_err($e))
//         }
//     }
// }


// macro_rules! pmatch {
//     ($s:expr, $p:pat, $do:block) => {
//         if let $p = &$s.tokens[$s.idx].t {
//             $s.idx += 1;
//             $do
//         } 
//     }
// }

// macro_rules! ptest {
//     ($s:expr, $($p:pat)|+, $do_match:block else $do_nomatch:block) => {
//             if let $($p)|+ = &$s.tokens[$s.idx].t {
//                 $s.idx += 1;
//                 $do_match
//             } else {
//                 $do_nomatch
//             }
//     }
// }


impl<'a> Parser<'a> {
    const EOF_ERROR:Error = Error{line:0, col:0, message:String::from("Unexpected end of file.")};
    // fn error_recover(&mut self) {
    //     self.idx += 1;
    //     while self.idx + 1 < self.tokens.len() {
    //         if let Token::Task{..} | Token::Method{..} | Token::CloseParenthesis{..}  = self.tokens[self.idx].t {
    //             return;
    //         }
    //         if let Token::StatementEnd{..} = self.tokens[self.idx].t {
    //             self.idx += 1;
    //             return;
    //         }
    //         self.idx += 1;
    //     }
    // }
    fn primary(&mut self) -> Result<Expr<'a>, Error> {
        // Literal | Label | "(" expression ")"
        match self.lexer.peek() {
            Some(Ok(Token{t:Literal(_),..})) => Ok(Expr::Literal(self.lexer.next().unwrap().unwrap())),
            Some(Ok(Token{t:Identifier(_),..})) => Ok(Expr::Variable(self.parse_object_path()?)),
            Some(Ok(Token{t:OpenParenthesis,..})) => {
                let token = self.lexer.next().unwrap().unwrap();
                let expr = self.expression()?;
                if self.lexer.next_if(|t| match t { Ok(Token{t:CloseParenthesis,..})=>true,_=>false}).is_some() {
                    Ok(Expr::Grouping(Box::new(expr), token))
                } else {
                    Err(token.to_err("Expected ')' after expression."))
                }
            },
            Some(Ok(token)) => Err(token.to_err("Expected expression.")),
            Some(Err(e)) => Err(self.lexer.next().unwrap().unwrap_err()),
            None => Err(Parser::EOF_ERROR),
        }
    }

    fn call(&mut self) -> Result<Expr<'a>, Error> {
        let expr = self.primary()?;
        if let Some(Ok(Token{t:OpenParenthesis,..})) = self.lexer.peek() {
            self.lexer.next();
            if let Expr::Variable(name) = expr {
                if name.len() == 1 {
                    let mut args = Vec::<Expr>::new();
                    loop {
                        if self.lexer.next_if(|t| match t { Ok(Token{t:CloseParenthesis,..})=>true,_=>false}).is_some() { 
                            break
                        }
                        args.push(self.expression()?);
                        match self.lexer.next() {
                            Some(Ok(Token{t:Comma,..})) => (),
                            Some(Ok(token)) => return Err(token.to_err("Expected ','.")),
                            Some(Err(e)) => return Err(e),
                            None => return Err(Parser::EOF_ERROR),
                        }
                    }
                    Ok(Expr::Call(name[0], args))
                } else {
                    Err(name[0].to_err("Can not call object properties."))
                }
            } else {
                Err(expr.to_err("Expected identifier."))
            }
        } else {
            Ok(expr)
        }
    }

    fn unary(&mut self) -> Result<Expr<'a>, Error> {
        if let Some(Ok(operator)) = self.lexer.next_if(|t| match t { Ok(Token{t:Not | Minus,..})=>true,_=>false}) {
            let right = self.unary()?;
            Ok(Expr::Unary(operator, Box::new(right)))
        } else {
            self.call()
        }
    }
    fn factor(&mut self) -> Result<Expr<'a>, Error> {
        let mut expr = self.unary()?;
        while let Some(Ok(operator)) = self.lexer.next_if(|t| match t { Ok(Token{t:Slash | Star,..})=>true,_=>false}) {
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn term(&mut self) -> Result<Expr<'a>, Error> {
        let mut expr = self.factor()?;
        while let Some(Ok(operator)) = self.lexer.next_if(|t| match t { Ok(Token{t:Minus | Plus,..})=>true,_=>false}) {
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn comparison(&mut self) -> Result<Expr<'a>, Error> {
        let mut expr = self.term()?;
        while let Some(Ok(operator)) = self.lexer.next_if(|t| match t { Ok(Token{t:Greater | GreaterOrEquals | Smaller | SmallerOrEquals,..})=>true,_=>false}) {
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn equality(&mut self) -> Result<Expr<'a>, Error> {
        let mut expr =self.comparison()?;
        while let Some(Ok(operator)) = self.lexer.next_if(|t| match t { Ok(Token{t:NotEquals | EqualsEquals,..})=>true,_=>false}) {
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn logic(&mut self) -> Result<Expr<'a>, Error> {
        let mut expr = self.equality()?;
        while let Some(Ok(operator)) = self.lexer.next_if(|t| match t { Ok(Token{t:Or | And,..})=>true,_=>false}) {
            let right = self.equality()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn assignment(&mut self) -> Result<Expr<'a>, Error> {
        let target = self.logic()?;
        if let Some(Ok(operator)) = self.lexer.next_if(|t| match t { Ok(Token{t:Equals | AddTo | SubtractFrom | DivideBy | MultiplyBy,..})=>true,_=>false}) {
            let target = self.parse_object_path()?;
            let line = operator.line;
            let col = operator.col;
            let value_expr = match operator {
                Token{t:AddTo,..} => Expr::Binary(Box::new(Expr::Variable(target.clone())), Token{line, col, len:0, t:Plus}, Box::new(self.expression()?)),
                Token{t:SubtractFrom,..} => Expr::Binary(Box::new(Expr::Variable(target.clone())), Token{line, col, len:0, t:Minus}, Box::new(self.expression()?)),
                Token{t:MultiplyBy,..} => Expr::Binary(Box::new(Expr::Variable(target.clone())), Token{line, col, len:0, t:Star}, Box::new(self.expression()?)),
                Token{t:DivideBy,..} => Expr::Binary(Box::new(Expr::Variable(target.clone())), Token{line, col, len:0, t:Slash}, Box::new(self.expression()?)),
                _ => self.expression()?,
            };
            Ok(Expr::Assignment(target, Box::new(value_expr)))
        } else {
            Ok(target)
        }
    }
    
    fn expression(&mut self) -> Result<Expr<'a>, Error> {
        if let Some(Ok(token)) = self.lexer.next_if(|t| match t {Ok(Token{t:Pass,..})=>true,_=>false}) {
            Ok(Expr::Nop(token))
        } else {
            self.assignment()
        }
    }

    fn parse_object_path(&mut self) -> Result<Vec<Token<'a>>, Error> {
        let mut name = vec![match self.lexer.next() {
            Some(Ok(token @ Token{t:Identifier(_),..})) => Ok(token),
            Some(Ok(token)) => Err(token.to_err("Expected identifier.")),
            Some(Err(e)) => Err(e),
            None => Err(Parser::EOF_ERROR),
        }?];
        while self.lexer.next_if(|t| match t { Ok(Token{t:Dot,..}) => true, _=>false}).is_some() {
            name.push(match self.lexer.next() {
                Some(Ok(token @ Token{t:Identifier(_),..})) => Ok(token),
                Some(Ok(token)) => Err(token.to_err("Expected identifier.")),
                Some(Err(e)) => Err(e),
                None => Err(Parser::EOF_ERROR),
            }?)
        }
        Ok(name)
    }
    fn parse_preconditions(&mut self) -> Result<Option<Expr<'a>>, Error> {
        if self.lexer.next_if(|t| match t { Ok(Token{t:OpenParenthesis,..})=>true, _=>false}).is_some() {
            let preconditions = self.expression()?;
            match self.lexer.next() {
                Some(Ok(Token{t:CloseParenthesis,..})) => Ok(Some(preconditions)),
                Some(Ok(token)) => Err(token.to_err("Expected ')' after preconditions.")),
                Some(Err(e)) => Err(e),
                None => Err(Parser::EOF_ERROR),
            }
        } else {
            Ok(None)
        }
    }

    fn parse_cost(&mut self) -> Result<Option<Expr<'a>>, Error> {
        if self.lexer.next_if(|t| match t { Ok(Token{t:Cost,..})=>true, _=>false}).is_some() {
            Ok(Some(self.expression()?))
        } else {
            Ok(None)
        }
    }

    fn parse_body(&mut self) -> Result<Stmt<'a>, Error> {
        match self.lexer.next() {
            Some(Ok(Token{t:Colon,..})) => Ok(self.statement()?),
            Some(Ok(token)) => Err(token.to_err("Expected ':'.")),
            Some(Err(e)) => Err(e),
            None => Err(Parser::EOF_ERROR),
        }
    }
    fn parse_binding(&mut self) -> Result<Option<(&'a str, &'a str)>, Error> {
        if let Some(Ok(for_token)) = self.lexer.next_if(|t| match t { Ok(Token{t:For,..})=>true, _=>false}) {
            let class = match self.lexer.next() {
                Some(Ok(Token{t:Identifier(class),..})) => Ok(class),
                Some(Ok(token)) => Err(token.to_err("Expected identifier.")),
                Some(Err(e)) => Err(e),
                None => Err(Parser::EOF_ERROR),
            }?;
            let variable = if self.lexer.next_if(|t| match t { Ok(Token{t:On,..})=>true,_=>false}).is_some() {
                match self.lexer.next() {
                    Some(Ok(Token{t:Identifier(variable),..})) => Ok(variable),
                    Some(Ok(token)) => Err(token.to_err("Expected identifier.")),
                    Some(Err(e)) => Err(e),
                    None => Err(Parser::EOF_ERROR),
                }
            } else {
                Err(for_token.to_err("Expected 'on' after class identifier."))
            }?;
            Ok(Some((class, variable)))
        } else {
            Ok(None)
        }
    }

    fn task_statement(&mut self) -> Result<Stmt<'a>, Error> {
        let name = self.parse_object_path()?;
        if let Some(Ok(Token{t:StatementEnd,..})) = self.lexer.peek() {
            Ok(Stmt::TaskDeclaration{name})
        } else {
            let binding = self.parse_binding()?;
            let preconditions = self.parse_preconditions()?;
            let cost = self.parse_cost()?;
            let body = Box::new(self.parse_body()?);
            let effects = if self.lexer.next_if(|t| match t { Ok(Token{t:Effects,..})=>true,_=>false}).is_some() {
                Some(Box::new(self.parse_body()?))
            } else {
                None
            };
            let planning = if self.lexer.next_if(|t| match t { Ok(Token{t:Planning,..})=>true,_=>false}).is_some() {
                Some(Box::new(self.parse_body()?))
            } else {
                None
            };
            Ok(Stmt::Task{name, preconditions, cost, body, binding, effects, planning})
        }
    }
        
    fn expression_statement(&mut self) -> Result<Stmt<'a>, Error> {
        let expr = self.expression()?;
        if self.lexer.next_if(|t| match t { Ok(Token{t:StatementEnd,..}) => true, _ => false } ).is_some() {
            Ok(Stmt::Expression(expr))
        } else {
            Err(expr.to_err("Expected new line after expression."))
        }
    }
    fn block_statement(&mut self) -> Result<Stmt<'a>, Error> {
        let mut stmts = Vec::<Stmt>::new();
        loop {
            stmts.push(self.statement()?);
            if self.lexer.next_if(|t| match t { Ok(Token{t:BlockEnd,..}) => true, _ => false }).is_some() {
                break Ok(Stmt::Block(stmts))
            } 
        }
        
    }
    fn include_statement(&mut self) -> Result<Stmt<'a>, Error> {
        let target = match self.lexer.next() {
            Some(Ok(token @ Token{t:Literal(S(src)),..})) => Ok(token),
            Some(Ok(token)) => Err(token.to_err("Expected string literal.")),
            Some(Err(e)) => Err(e),
            None => Err(Parser::EOF_ERROR),
        }?;
        if self.lexer.next_if(|t| match t { Ok(Token{t:StatementEnd,..}) => true, _ => false}).is_some() {
            Ok(Stmt::Include(target))
        } else {
            Err(target.to_err("Expected new line after expression."))
        }
    }
    fn type_statement(&mut self) -> Result<Stmt<'a>, Error> {
        let name = match self.lexer.next() {
            Some(Ok(token @ Token{t:Identifier(_),..})) => Ok(token),
            Some(Ok(token)) => Err(token.to_err("Expected type name.")),
            Some(Err(e)) => Err(e),
            None => Err(Parser::EOF_ERROR),
        }?;
        let body = Box::new(self.parse_body()?);
        Ok(Stmt::Type{name, body})
    }

    fn statement(&mut self)-> Result<Stmt<'a>, Error> {
        let stmt = match self.lexer.next() {
            Some(Ok(Token{t:Task,..})) => self.task_statement(),
            Some(Ok(Token{t:BlockStart,..})) => self.block_statement(),
            Some(Ok(Token{t:Type,..})) => self.type_statement(),
            Some(Ok(Token{t:Include,..})) => self.include_statement(),
            Some(Err(e)) => Err(e),
            None => Err(Parser::EOF_ERROR),
            _ => self.expression_statement()
        }?;
        match self.lexer.next() {
            Some(Ok(Token{t:StatementEnd,..})) => Ok(stmt),
            Some(Ok(token)) => Err(token.to_err("Expected new line after statement.")),
            Some(Err(e)) => Err(e),
            None => Err(Parser::EOF_ERROR),
        }
    }

    pub fn new(code:&'a str) -> Self {
        let lexer = Lexer::<'a>::new(code).peekable();
        Self{lexer}
    }
}