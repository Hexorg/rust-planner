use std::fmt;

pub mod tokens;
use tokens::{TokenData, Token, Literal};

pub mod lexer;
use lexer::Lexer;

pub mod expression;
use expression::Expr;

pub mod statement;
use statement::{Stmt, Binding};



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



pub struct Parser {
    idx: usize,
    tokens: Vec<Token>,
    ast: Vec<Stmt>,
    errors: Vec<Error>
}

macro_rules! pexpect {
    ($s:expr, $($p:pat)|+, $do:block, $e:literal) => {
        if let $($p)|+ = &$s.tokens[$s.idx].t {
            $s.idx += 1;
            Ok($do)
        } else {
            Err($s.tokens[$s.idx].to_err($e))
        }
    }
}


// macro_rules! pmatch {
//     ($s:expr, $p:pat, $do:block) => {
//         if let $p = &$s.tokens[$s.idx].t {
//             $s.idx += 1;
//             $do
//         } 
//     }
// }

macro_rules! ptest {
    ($s:expr, $($p:pat)|+, $do_match:block else $do_nomatch:block) => {
            if let $($p)|+ = &$s.tokens[$s.idx].t {
                $s.idx += 1;
                $do_match
            } else {
                $do_nomatch
            }
    }
}


impl Parser {
    fn error_recover(&mut self) {
        self.idx += 1;
        while self.idx + 1 < self.tokens.len() {
            if let TokenData::Task | TokenData::Method | TokenData::CloseParenthesis  = self.tokens[self.idx].t {
                return;
            }
            if let TokenData::StatementEnd = self.tokens[self.idx].t {
                self.idx += 1;
                return;
            }
            self.idx += 1;
        }
    }
    fn primary(&mut self) -> Result<Expr, Error> {
        // Literal | "(" expression ")"
        if let TokenData::Literal(_)  = self.tokens[self.idx].t {
            self.idx += 1;
            Ok(Expr::Literal(self.tokens[self.idx-1].clone()))
        } else if let TokenData::OpenParenthesis = self.tokens[self.idx].t {
            self.idx += 1;
            let expr = self.expression()?;
            pexpect!(self, TokenData::CloseParenthesis, {
                Expr::Grouping(Box::new(expr), self.tokens[self.idx].clone())
            }, "Expected ')' after expression.")
        } else if let TokenData::Label(_) = &self.tokens[self.idx].t {
            
            let mut var_tokens = Vec::<Token>::new();
            loop {
                var_tokens.push(self.tokens[self.idx].clone());
                self.idx += 1;
                if let TokenData::Dot = &self.tokens[self.idx].t { 
                    self.idx += 1;
                    if let TokenData::Label(_) = &self.tokens[self.idx].t {
                         Ok(()) 
                    } else {
                        Err(self.tokens[self.idx].to_err("Expected label after '.'."))
                    }?;
                }
                else { break }
            }
            Ok(Expr::Variable(var_tokens))
        } else {
            Err(self.tokens[self.idx].to_err("Expected expression."))
        }

    }
    fn call(&mut self) -> Result<Expr, Error> {
        let expr = self.primary()?;
        ptest!(self, TokenData::OpenParenthesis, {
            let mut args = Vec::<Expr>::new();
            loop { 
                ptest!(self, TokenData::CloseParenthesis, {
                    break if let Expr::Variable(name) = expr {
                        if name.len() == 1 {
                            Ok(Expr::Call(name.get(0).unwrap().clone(), args))
                        } else {
                            Err(self.tokens[self.idx].to_err("Can not call object methods."))
                        }
                    } else {
                        Err(self.tokens[self.idx-1].to_err("Expected function name before call."))
                    }
                } else {
                    args.push(self.expression()?);
                    ptest!(self, TokenData::Comma, {} else {});
                })
            }
        } else {
            Ok(expr)
        })
    }
    fn unary(&mut self) -> Result<Expr, Error> {
        ptest!(self, TokenData::Not | TokenData::Minus, {
            let operator = self.tokens[self.idx-1].clone();
            let right = self.unary()?;
            Ok(Expr::Unary(operator, Box::new(right)))
        } else {
            self.call()
        }) 
    }
    fn factor(&mut self) -> Result<Expr, Error> {
        let mut expr = self.unary()?;
        while let TokenData::Slash | TokenData::Star  = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn term(&mut self) -> Result<Expr, Error> {
        let mut expr = self.factor()?;
        while let TokenData::Minus | TokenData::Plus  = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn comparison(&mut self) -> Result<Expr, Error> {
        let mut expr = self.term()?;
        while let TokenData::Greater | TokenData::GreaterOrEquals | TokenData::Smaller | TokenData::SmallerOrEquals = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn equality(&mut self) -> Result<Expr, Error> {
        let mut expr = self.comparison()?;
        while let TokenData::NotEquals | TokenData::EqualsEquals = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn logic(&mut self) -> Result<Expr, Error> {
        let mut expr = self.equality()?;
        while let TokenData::Or | TokenData::And = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.equality()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn assignment(&mut self) -> Result<Expr, Error> {
        let target = self.logic()?;
        ptest!(self, TokenData::Equals 
            | TokenData::AddTo  
            | TokenData::SubtractFrom
            | TokenData::DivideBy
            | TokenData::MultiplyBy, {
                if let Expr::Variable(varname) = target {
                    let line = self.tokens[self.idx-1].line;
                    let col = self.tokens[self.idx-1].col;
                    let value_expr = match self.tokens[self.idx-1].t {
                        // TokenData::EQUALS => self.expression()?,
                        TokenData::AddTo => Expr::Binary(Box::new(Expr::Variable(varname.clone())), Token{line, col, len:0, t:TokenData::Plus}, Box::new(self.expression()?)),
                        TokenData::SubtractFrom => Expr::Binary(Box::new(Expr::Variable(varname.clone())), Token{line, col, len:0, t:TokenData::Minus}, Box::new(self.expression()?)),
                        TokenData::MultiplyBy => Expr::Binary(Box::new(Expr::Variable(varname.clone())), Token{line, col, len:0, t:TokenData::Star}, Box::new(self.expression()?)),
                        TokenData::DivideBy => Expr::Binary(Box::new(Expr::Variable(varname.clone())), Token{line, col, len:0, t:TokenData::Slash}, Box::new(self.expression()?)),
                        _ => self.expression()?,
                    };
                    Ok(Expr::Assignment(varname, Box::new(value_expr)))
                } else {
                    let line = self.tokens[self.idx].line;
                    let col = self.tokens[self.idx].col;
                    Err(Error{line, col, message:String::from("Invalid assignment target.")})
                }
        } else {
            Ok(target)
        })
    }
    fn expression(&mut self) -> Result<Expr, Error> {
        if let TokenData::Pass = &self.tokens[self.idx].t {
            self.idx += 1;
            Ok(Expr::Noop(self.tokens[self.idx-1].clone()))
        } else {
            match self.assignment() {
                Ok(e) => Ok(e),
                Err(e) => {self.errors.push(e); self.error_recover(); Ok(Expr::Noop(self.tokens[self.idx].clone()))}
            }
        }
    }
    fn parse_preconditions(&mut self) -> Result<Option<Expr>, Error> {
        ptest!(self, TokenData::OpenParenthesis, {
            let preconditions = Some(self.expression()?);
            pexpect!(self, TokenData::CloseParenthesis, {preconditions}, "Expected ')' after task conditions.")
        } else {
            Ok(None)
        })
    }

    fn parse_cost(&mut self) -> Result<Option<i32>, Error> {
        ptest!(self, TokenData::Cost, {
            pexpect!(self, TokenData::Literal(Literal::I(cost)), { Some(*cost) }, "Expected integer after cost.")
        } else { 
            Ok(None)
        })
    }

    fn parse_body(&mut self) -> Result<Stmt, Error> {
        pexpect!(self, TokenData::Colon, {self.statement()?}, "Expected ':'")
    }

    fn task_statement(&mut self) -> Result<Stmt, Error> {
        // let token_id = self.idx-1;
        pexpect!(self, TokenData::Label(_), {
            let name: Token = self.tokens[self.idx-1].clone();
            let mut preconditions = self.parse_preconditions()?; 
            let binding = ptest!(self, TokenData::On, {
                let class_type = pexpect!(self, TokenData::Label(s), {s}, "Expected label after on.")?.clone();
                pexpect!(self, TokenData::As, {()}, "Expected 'as'.")?;
                let variable_name = pexpect!(self, TokenData::Label(s), {s}, "Exptected label after as.")?.clone();
                Ok(Some(Binding{class_type, variable_name}))      
            } else { Ok(None) })?;
            let cost = self.parse_cost()?;
            let body = Box::new(self.parse_body()?);
            let effects = ptest!(self, TokenData::Effects, {Ok(Some(Box::new(self.parse_body()?)))} else {Ok(None)})?;
            Stmt::Task{name, preconditions, cost, body, binding, effects}
        }, "Expected label after 'task'.")
        
    }
    fn expression_statement(&mut self) -> Result<Stmt, Error> {
        let expr = self.expression()?;
        pexpect!(self, TokenData::StatementEnd | TokenData::BlockStart | TokenData::BlockEnd, 
            {Stmt::Expression(expr)}, 
            "Expected new line after expression.")
    }
    fn block_statement(&mut self) -> Result<Stmt, Error> {
        let mut stmts = Vec::<Stmt>::new();
        loop {
            let stmt = self.statement();
            match stmt {
                Ok(stmt) => if let Stmt::Block(inner) = stmt {
                    stmts.extend(inner);
                } else {
                    stmts.push(stmt);
                },
                Err(e) => {self.errors.push(e); self.error_recover()}
            }
            

            if let TokenData::BlockEnd | TokenData::EOF = self.tokens[self.idx].t {
                self.idx += 1;
                return Ok(Stmt::Block(stmts))
            } 
        }
        
    }
    fn method_statement(&mut self) -> Result<Stmt, Error> {
        pexpect!(self, TokenData::Label(_), {
            let mut name: Token = self.tokens[self.idx-1].clone();
            let mut preconditions = self.parse_preconditions()?;
            let cost = self.parse_cost()?;
            let body = Box::new(self.parse_body()?);
            let mut else_cost = None;
            let else_body = ptest!(self, TokenData::Else, {
                else_cost = self.parse_cost()?;
                Ok(Some(Box::new(self.parse_body()?)))
            } else {
                Ok(None)
            })?;
            Stmt::Method{name, preconditions, cost, body, else_cost, else_body}
        }, "Expected method name.")
    }
    fn include_statement(&mut self) -> Result<Stmt, Error> {
        let t = self.tokens[self.idx].clone();
        self.idx += 1;
        pexpect!(self, TokenData::StatementEnd, {Stmt::Include(t)}, "Expected new line after expression.")
    }
    fn type_statement(&mut self) -> Result<Stmt, Error> {
        pexpect!(self, TokenData::Label(_), {
            let name: Token = self.tokens[self.idx-1].clone();
            let body = Box::new(self.parse_body()?);
            Stmt::Type{name, body}
        }, "Expected type name.")
    }
    fn statement(&mut self)-> Result<Stmt, Error> {
        // println!("When Parsing a new statement, next token is {}.", self.tokens[self.idx]);
        let r = if let TokenData::Task = self.tokens[self.idx].t {
            self.idx += 1;
            self.task_statement()
        } else if let TokenData::BlockStart = self.tokens[self.idx].t {
            self.idx += 1;
            self.block_statement()
        } else if let TokenData::Method = self.tokens[self.idx].t {
            self.idx += 1;
            self.method_statement()
        } else if let TokenData::Type = self.tokens[self.idx].t {
            self.idx += 1;
            self.type_statement()
        } else if let TokenData::Include = self.tokens[self.idx].t {
            self.idx += 1;
            self.include_statement()
        } else if let TokenData::EOF = self.tokens[self.idx].t {
            Err(self.tokens[self.idx].to_err("Reached the end of file"))
        } else {
            self.expression_statement()
        };
        // println!("After parsing a statement, next token is {}.", self.tokens[self.idx]);
        r
    }
    fn print_tokens(tokens: &Vec<Token>) {
        let mut depth = 0;
        println!("{}", tokens.iter().fold(String::new(), |acc, item| {
                let item_string = match item.t {
                    TokenData::BlockStart => {depth += 1; format!("{:?}", item.t)},
                    TokenData::BlockEnd => {format!("{:?}\n", item.t)},
                    TokenData::Colon => format!("{:?}\n", item.t),
                    _ => format!("{:?} ", item.t),
                };
                let r = if acc.ends_with('\n') || acc.len() == 0 {
                    acc + &format!("{:4}: {:width$}{}", item.line, ' ', item_string, width=depth*4)
                } else if acc.ends_with('{') {
                    acc + &format!(" {}", item_string)
                } else {
                    acc + &format!("{}", item_string)
                };
                if let TokenData::BlockEnd = item.t {
                    depth -= 1;
                };
                r
            }
        ));
    }
    pub fn parse(htn_source: &str) -> Result<Vec<Stmt>, Vec<Error>> {
        let mut parser = match Lexer::tokenize(htn_source) {
            Ok(tokens) => Parser{idx:0, tokens, ast:Vec::new(), errors:Vec::new()},
            Err(e) => { return Err(vec![e]); }
        };
        // Parser::print_tokens(&parser.tokens);
        while parser.idx + 1 < parser.tokens.len() {
            match parser.statement() {
                Ok(s) => parser.ast.push(s),
                Err(e) => {parser.errors.push(e); parser.error_recover()}
            }
        }
        if parser.errors.len() > 0 {
            Err(parser.errors)
        } else {
            Ok(parser.ast)
        }
    }
}