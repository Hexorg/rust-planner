use std::{fmt, string::ParseError, rc::Rc};

use super::domain::Domain;

pub struct ParserError {
    line: usize,
    col: usize,
    message: String
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line:{} col:{} {}", self.line, self.col, self.message)
    }
}
impl fmt::Debug for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line:{} col:{} {}", self.line, self.col, self.message)
    }
}

#[derive(Debug, Clone)]
pub enum TokenData {
    TASK,
    METHOD,
    ELSE,
    EFFECTS,
    LABEL(String),
    LITERAL(i32),
    EQUALS,
    EQUALS_EQUALS,
    MINUS,
    PLUS,
    SLASH,
    STAR,
    GREATER,
    SMALLER,
    GREATER_OR_EQUALS,
    SMALLER_OR_EQUALS,
    NOT_EQUALS,
    SUBTRACT_FROM,
    ADD_TO,
    MULTIPLY_BY,
    DIVIDE_BY,
    OR,
    AND,
    NOT,
    OPEN_PAREN,
    CLOSE_PAREN,
    COLON,
    BLOCK_START,
    BLOCK_END,
    NEW_LINE,
    EOF

}

#[derive(Clone, Debug)]
pub struct Token {
    line: usize,
    col: usize,
    // tab_depth: usize,
    t: TokenData,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ", self.t)?;
        if let TokenData::NEW_LINE = self.t {
            write!(f, "{:.4}: ", self.line)
        } else {
            Ok(())
        }
        
    }
}

impl fmt::Display for TokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenData::TASK => write!(f, "task"),
            TokenData::METHOD => write!(f, "method"),
            TokenData::ELSE => write!(f, "else"),
            TokenData::EFFECTS => write!(f, "effects"),
            TokenData::LABEL(l) => write!(f, "{}", l),
            TokenData::LITERAL(l) => write!(f, "{}", l),
            TokenData::EQUALS => write!(f, "="),
            TokenData::EQUALS_EQUALS => write!(f, "=="),
            TokenData::MINUS => write!(f, "-"),
            TokenData::PLUS => write!(f, "+"),
            TokenData::SLASH => write!(f, "/"),
            TokenData::STAR => write!(f, "*"),
            TokenData::GREATER => write!(f, ">"),
            TokenData::SMALLER => write!(f, "<task>"),
            TokenData::GREATER_OR_EQUALS => write!(f, ">="),
            TokenData::SMALLER_OR_EQUALS => write!(f, "<=>"),
            TokenData::NOT_EQUALS => write!(f, "!="),
            TokenData::SUBTRACT_FROM => write!(f, "-="),
            TokenData::ADD_TO => write!(f, "+="),
            TokenData::MULTIPLY_BY => write!(f, "*="),
            TokenData::DIVIDE_BY => write!(f, "/="),
            TokenData::OR => write!(f, " | "),
            TokenData::AND => write!(f, " & "),
            TokenData::NOT => write!(f, "!"),
            TokenData::OPEN_PAREN => write!(f, "("),
            TokenData::CLOSE_PAREN => write!(f, ")"),
            TokenData::COLON => write!(f, ":"),
            TokenData::BLOCK_START => write!(f, "{{"),
            TokenData::BLOCK_END => write!(f, "}}"),
            TokenData::NEW_LINE => write!(f, "\\n\n"),
            TokenData::EOF => write!(f, "<<EOF"),
        }
    }
}

enum DepthSeparator {
    TABS,
    SPACES(usize)
}

pub struct Lexer {

}

pub struct Parser<'a> {
    idx: usize,
    tokens: &'a Vec<Token>,
}

impl Lexer {
    pub fn tokenize(htn_source: &str) -> Result<Vec<Token>, ParserError> {
        let mut line = 1;
        let mut col = 1;
        let mut last_line_tab_depth = 0;
        let mut tab_depth = 0;
        let mut depth_separator : Option<DepthSeparator> = None;
        let mut is_comment = false;
        let mut is_newline = true;
        let mut r = Vec::<Token>::new();
        // let mut accumulator = String::new();
        for c in htn_source.chars() {
            if is_comment && c != '\n' {
                continue;
            }
            match c {
                '#' => is_comment = true,
                ':' => r.push(Token{line, col, t:TokenData::COLON}),
                '(' => r.push(Token{line, col, t:TokenData::OPEN_PAREN}),
                ')' => r.push(Token{line, col, t:TokenData::CLOSE_PAREN}),
                '=' => r.push(Token{line, col, t:TokenData::EQUALS}),
                '-' => r.push(Token{line, col, t:TokenData::MINUS}),
                '+' => r.push(Token{line, col, t:TokenData::PLUS}),
                '/' => r.push(Token{line, col, t:TokenData::SLASH}),
                '*' => r.push(Token{line, col, t:TokenData::STAR}),
                '>' => r.push(Token{line, col, t:TokenData::GREATER}),
                '<' => r.push(Token{line, col, t:TokenData::SMALLER}),
                '!' => r.push(Token{line, col, t:TokenData::NOT}),
                '|' => r.push(Token{line, col, t:TokenData::OR}),
                '&' => r.push(Token{line, col, t:TokenData::AND}),
                '\n' => { if let Some(Token{t:TokenData::NEW_LINE,..}) = r.last() {
                    // println!("Double new line at line:{} col:{} last_line_tab_depth:{}", line, col, last_line_tab_depth);
                        while last_line_tab_depth > 0 {
                            r.push(Token{line, col, t:TokenData::BLOCK_END});
                            last_line_tab_depth -= 1;
                        }
                    }
                    r.push(Token{line, col, t:TokenData::NEW_LINE}); 
                    is_comment = false; 
                    is_newline = true;
                    last_line_tab_depth = tab_depth;
                    tab_depth = 0;
                    line += 1; 
                    col = 1; 
                    // tab_depth = 0;
                },
                // c if (c.is_alphabetic() && r.len() == 0) => r.push(Token{line, col, t:TokenData::LABEL(String::from(c))}),
                c if !c.is_whitespace() => {
                    if is_newline {
                        if let Some(DepthSeparator::SPACES(0)) = depth_separator {
                            depth_separator = Some(DepthSeparator::SPACES(tab_depth));
                            tab_depth = 1;
                        } else if let Some(DepthSeparator::SPACES(sc)) = depth_separator {
                            if tab_depth % sc != 0 {
                                return Err(ParserError{line, col, message:String::from("Unexpected amount of spaces")});
                            }
                            tab_depth = tab_depth / sc;
                        }
                        if tab_depth > last_line_tab_depth {
                            r.push(Token{line, col:1, t:TokenData::BLOCK_START});
                        } else if tab_depth < last_line_tab_depth { 
                            r.push(Token{line, col:1, t:TokenData::BLOCK_END});
                        }
                        is_newline = false;
                    }
                    let last_token = r.last_mut();
                    if let Some(Token{t:TokenData::LABEL(ref mut label), ..}) = last_token {
                        label.push(c);
                        match label.as_str() {
                            "task" => last_token.unwrap().t = TokenData::TASK,
                            "method" => last_token.unwrap().t = TokenData::METHOD,
                            "else" => last_token.unwrap().t = TokenData::ELSE,
                            "effects" => last_token.unwrap().t = TokenData::EFFECTS,
                            "or" => last_token.unwrap().t = TokenData::OR,
                            "and" => last_token.unwrap().t = TokenData::AND,
                            "not" => last_token.unwrap().t = TokenData::NOT,
                            "false" => last_token.unwrap().t = TokenData::LITERAL(0),
                            "true" => last_token.unwrap().t = TokenData::LITERAL(1),
                            _ => (),
                        }
                    } else {

                        r.push(Token{line, col, t:TokenData::LABEL(String::from(c))})
                    }
                },
                '\t' if is_newline => { if depth_separator.is_none() {
                        depth_separator = Some(DepthSeparator::TABS);
                    } else if let Some(DepthSeparator::SPACES(_)) = depth_separator {
                        return Err(ParserError{line, col, message:String::from("Tabs and spaces can't be used together")})
                    }
                    tab_depth += 1;
                }
                c if c.is_whitespace() && is_newline => { if depth_separator.is_none() {
                        depth_separator = Some(DepthSeparator::SPACES(0));
                    } else if let Some(DepthSeparator::TABS) = depth_separator {
                        return Err(ParserError{line, col, message:String::from("Tabs and spaces can't be used together")})
                    }
                    tab_depth += 1;
                }
                _ => (),
            }
            col += 1;
            if let Some(Token{t:TokenData::EQUALS,..}) = r.last() {
                match r.get(r.len()-2) {
                    Some(Token{t:TokenData::EQUALS,..}) => {r.pop(); r.last_mut().unwrap().t = TokenData::EQUALS_EQUALS},
                    Some(Token{t:TokenData::SMALLER,..}) => {r.pop(); r.last_mut().unwrap().t = TokenData::SMALLER_OR_EQUALS},
                    Some(Token{t:TokenData::GREATER,..}) => {r.pop(); r.last_mut().unwrap().t = TokenData::GREATER_OR_EQUALS},
                    Some(Token{t:TokenData::NOT,..}) => {r.pop(); r.last_mut().unwrap().t = TokenData::NOT_EQUALS},
                    Some(Token{t:TokenData::MINUS,..}) => {r.pop(); r.last_mut().unwrap().t = TokenData::SUBTRACT_FROM},
                    Some(Token{t:TokenData::PLUS,..}) => {r.pop(); r.last_mut().unwrap().t = TokenData::ADD_TO},
                    Some(Token{t:TokenData::SLASH,..}) => {r.pop(); r.last_mut().unwrap().t = TokenData::DIVIDE_BY},
                    Some(Token{t:TokenData::STAR,..}) => {r.pop(); r.last_mut().unwrap().t = TokenData::MULTIPLY_BY},
                    _ => (),
                }
            }
            if let Some(Token{t:TokenData::LABEL(label),..}) = r.last() {
                if let Ok(literal) = label.parse::<i32>() {
                    r.last_mut().unwrap().t = TokenData::LITERAL(literal);
                }
            }
        }
        r.push(Token{line, col, t:TokenData::EOF});
        Ok(r)
    }
}
#[derive(Clone)]
enum Expr {
    Binary(Rc<Expr>, Token, Rc<Expr>), // left Token right
    Grouping(Rc<Expr>), // group of expressions
    Literal(i32),
    Variable(String),
    Unary(Token, Rc<Expr>) // Token right
}

enum Stmt {
    Method(String, Option<Expr>, Rc<Stmt>), // Name, condition, subtasks
    Task(String, Option<Expr>, Rc<Stmt>, Option<Rc<Stmt>>), // Name, condition, methods, effects
    Assignment(String, Expr), // destination, expression
    Call(String, Vec<Expr>),
    Block(Vec<Stmt>),
    Expression(Expr)
}
impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binary(arg0, arg1, arg2) => write!(f, "({:?} {} {:?})", arg0, arg1.t, arg2),
            Self::Grouping(arg0) => write!(f, "({:?})", arg0),
            Self::Literal(arg0) => write!(f, "{}", arg0),
            Self::Variable(arg0) => write!(f, "{}", arg0),
            Self::Unary(arg0, arg1) => f.debug_tuple("Unary").field(arg0).field(arg1).finish(),
        }
    }
}

impl fmt::Debug for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Method(arg0, arg1, arg2) => f.debug_tuple("Method").field(arg0).field(arg1).field(arg2).finish(),
            Self::Task(arg0, arg1, arg2, arg3) => f.debug_tuple("Task").field(arg0).field(arg1).field(arg2).field(arg3).finish(),
            Self::Assignment(arg0, arg1) => f.debug_tuple("Assignment").field(arg0).field(arg1).finish(),
            Self::Call(arg0, arg1) => f.debug_tuple("Call").field(arg0).field(arg1).finish(),
            Self::Block(arg0) => f.debug_tuple("Block").field(arg0).finish(),
            Stmt::Expression(arg0) => write!(f, "{:?}", arg0),
        }
    }
}

impl Parser<'_> {
    fn error_recover(&mut self) {
        self.idx += 1;
        while self.idx + 1 != self.tokens.len() {
            if let TokenData::NEW_LINE = self.tokens[self.idx-1].t {
                return;
            }
            if let TokenData::TASK | TokenData::METHOD | TokenData::EFFECTS = self.tokens[self.idx].t {
                return;
            }
            self.idx += 1;
        }
    }
    fn unary(&mut self) -> Result<Expr, ParserError> {
        if let TokenData::NOT | TokenData::MINUS = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.unary()?;
            Ok(Expr::Unary(operator, Rc::new(right)))
        } else {
            self.primary()
        }   
    }
    fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;
        while let TokenData::SLASH | TokenData::STAR = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.unary()?;
            expr = Expr::Binary(Rc::new(expr), operator, Rc::new(right));
        }
        Ok(expr)
    }
    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;
        while let TokenData::MINUS | TokenData::PLUS = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.factor()?;
            expr = Expr::Binary(Rc::new(expr), operator, Rc::new(right));
        }
        Ok(expr)
    }
    fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.term()?;
        while let TokenData::GREATER | TokenData::GREATER_OR_EQUALS | TokenData::SMALLER | TokenData::SMALLER_OR_EQUALS = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.term()?;
            expr = Expr::Binary(Rc::new(expr), operator, Rc::new(right));
        }
        Ok(expr)
    }
    fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;
        while let TokenData::NOT_EQUALS | TokenData::EQUALS_EQUALS = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.comparison()?;
            expr = Expr::Binary(Rc::new(expr), operator, Rc::new(right));
        }
        Ok(expr)
    }
    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.equality()
    }
    fn primary(&mut self) -> Result<Expr, ParserError> {
        // Literal | "(" expression ")"
        if let TokenData::LITERAL(val)  = self.tokens[self.idx].t {
            self.idx += 1;
            Ok(Expr::Literal(val))
        } else if let TokenData::OPEN_PAREN = self.tokens[self.idx].t {
            self.idx += 1;
            let expr = self.expression()?;
            if let TokenData::CLOSE_PAREN = self.tokens[self.idx].t {
                self.idx += 1;
                Ok(Expr::Grouping(Rc::new(expr)))
            } else {
                let line = self.tokens[self.idx].line;
                let col = self.tokens[self.idx].col;
                Err(ParserError{line, col, message:String::from("Expected ')' after expression.")})
            }
        } else if let TokenData::LABEL(name) = &self.tokens[self.idx].t {
            self.idx += 1;
            Ok(Expr::Variable(name.clone()))
        } else {
            let line = self.tokens[self.idx].line;
            let col = self.tokens[self.idx].col;
            Err(ParserError{line, col, message:String::from("Expected expression.")}).unwrap()
        }

    }
    fn effects_statement(&mut self) -> Result<Stmt, ParserError> {
        if let TokenData::COLON = self.tokens[self.idx].t {
            self.idx += 1;
            if let TokenData::NEW_LINE = self.tokens[self.idx].t {
                self.idx += 1;
                self.statement()
            } else {
                let line = self.tokens[self.idx].line;
                let col = self.tokens[self.idx].col;
                Err(ParserError{line, col, message:String::from("Expected new line after effects statement.")})
            }
        } else {
            let line = self.tokens[self.idx].line;
            let col = self.tokens[self.idx].col;
            Err(ParserError{line, col, message:String::from("Expected ':' after 'effects'.")})
        }
    }
    fn task_statement(&mut self) -> Result<Stmt, ParserError> {
        if let TokenData::LABEL(name) = &self.tokens[self.idx].t {
            self.idx += 1;
            let mut conditions : Option<Expr> = None;
            if let TokenData::OPEN_PAREN = self.tokens[self.idx].t {
                self.idx += 1;
                conditions = Some(self.expression()?);
                if let TokenData::CLOSE_PAREN = self.tokens[self.idx].t {
                    self.idx += 1;
                } else {
                    let line = self.tokens[self.idx].line;
                    let col = self.tokens[self.idx].col;
                    return Err(ParserError{line, col, message:String::from("Expected ')' after task conditions.")})
                }
            }
            if let TokenData::COLON = self.tokens[self.idx].t {
                self.idx += 1;
                if let TokenData::NEW_LINE = self.tokens[self.idx].t {
                    self.idx += 1;
                    let task_block = self.statement()?;
                    let mut effects_block = None;
                    if let TokenData::EFFECTS = self.tokens[self.idx].t {
                        self.idx += 1;
                        effects_block = Some(Rc::new(self.effects_statement()?));
                    }
                    Ok(Stmt::Task(name.clone(), conditions, Rc::new(task_block), effects_block))
                } else {
                    let line = self.tokens[self.idx].line;
                    let col = self.tokens[self.idx].col;
                    Err(ParserError{line, col, message:String::from("Expected new line after task statement.")})
                }
            } else {
                let line = self.tokens[self.idx].line;
                let col = self.tokens[self.idx].col;
                Err(ParserError{line, col, message:String::from("Expected ':' after task label.")})
            }
        } else {
            let line = self.tokens[self.idx].line;
            let col = self.tokens[self.idx].col;
            Err(ParserError{line, col, message:String::from("Expected label after 'task'.")})
        }
    }
    fn expression_statement(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;
        if let TokenData::NEW_LINE | TokenData::EOF  = self.tokens[self.idx].t {
            self.idx += 1;
            Ok(Stmt::Expression(expr))
        } else {
            let line = self.tokens[self.idx].line;
            let col = self.tokens[self.idx].col;
            Err(ParserError{line, col, message:String::from("Expected new line after expression.")})
        }
    }
    fn call_statement(&mut self) -> Result<Stmt, ParserError> {
        if let TokenData::LABEL(name) = &self.tokens[self.idx].t {
            self.idx += 1;
            if let TokenData::OPEN_PAREN = self.tokens[self.idx].t {
                self.idx += 1;
                let mut args = Vec::<Expr>::new();
                loop { 
                    if let TokenData::CLOSE_PAREN = self.tokens[self.idx].t {
                        self.idx += 1;
                        break
                    } else {
                        args.push(self.expression()?);
                    }
                }
                if let TokenData::NEW_LINE | TokenData::EOF = self.tokens[self.idx].t {
                    self.idx += 1;
                } else {
                    let line = self.tokens[self.idx].line;
                    let col = self.tokens[self.idx].col;
                    return Err(ParserError{line, col, message:String::from("Expected new line after call.")})
                }
                Ok(Stmt::Call(name.clone(), args))
            } else {
                let line = self.tokens[self.idx].line;
                let col = self.tokens[self.idx].col;
                Err(ParserError{line, col, message:String::from("Expected '(' after task name.")})
            }
        } else {
            let line = self.tokens[self.idx].line;
            let col = self.tokens[self.idx].col;
            Err(ParserError{line, col, message:String::from("Expected task identifier.")})
        }
    }
    fn assignment_statement(&mut self) -> Result<Stmt, ParserError> {
        if let TokenData::LABEL(name) = &self.tokens[self.idx].t {
            self.idx += 1;
            match self.tokens[self.idx].t {
                TokenData::EQUALS => {self.idx += 1; Ok(Stmt::Assignment(name.clone(), self.expression()?))},
                TokenData::ADD_TO => {self.idx += 1; Ok(Stmt::Assignment(name.clone(), Expr::Binary(Rc::new(Expr::Variable(name.clone())), Token{line:0, col:0, t:TokenData::PLUS}, Rc::new(self.expression()?))))},
                TokenData::SUBTRACT_FROM => {self.idx += 1; Ok(Stmt::Assignment(name.clone(), Expr::Binary(Rc::new(Expr::Variable(name.clone())), Token{line:0, col:0, t:TokenData::MINUS}, Rc::new(self.expression()?))))},
                TokenData::MULTIPLY_BY => {self.idx += 1; Ok(Stmt::Assignment(name.clone(), Expr::Binary(Rc::new(Expr::Variable(name.clone())), Token{line:0, col:0, t:TokenData::STAR}, Rc::new(self.expression()?))))},
                TokenData::DIVIDE_BY => {self.idx += 1; Ok(Stmt::Assignment(name.clone(), Expr::Binary(Rc::new(Expr::Variable(name.clone())), Token{line:0, col:0, t:TokenData::SLASH}, Rc::new(self.expression()?))))},
                _ => {
                    let line = self.tokens[self.idx].line;
                    let col = self.tokens[self.idx].col;
                    Err(ParserError{line, col, message:String::from("Expected '=' after variable name.")})
                }
            }
        } else {
            let line = self.tokens[self.idx].line;
            let col = self.tokens[self.idx].col;
            Err(ParserError{line, col, message:String::from("Expected identifier to assign to.")})
        }
    }
    fn block_statement(&mut self) -> Result<Stmt, ParserError> {

        let mut stmts = Vec::<Stmt>::new();
        loop {
            stmts.push(self.statement()?);
            if let TokenData::BLOCK_END | TokenData::EOF = self.tokens[self.idx].t {
                self.idx += 1;
                return Ok(Stmt::Block(stmts))
            } 
        }
        
    }
    fn method_statement(&mut self) -> Result<Stmt, ParserError> {
        if let TokenData::LABEL(name) = &self.tokens[self.idx].t {
            self.idx += 1;
            let mut conditions: Option<Expr> = None;
            if let TokenData::OPEN_PAREN = self.tokens[self.idx].t {
                self.idx += 1;
                conditions = Some(self.expression()?);
                if let TokenData::CLOSE_PAREN = self.tokens[self.idx].t {
                    self.idx += 1;
                } else {
                    let line = self.tokens[self.idx].line;
                    let col = self.tokens[self.idx].col;
                    return Err(ParserError{line, col, message:String::from("Expected ')' after method conditions.'")})
                }
            }
            if let TokenData::COLON = self.tokens[self.idx].t {
                self.idx += 1;
                if let TokenData::NEW_LINE = self.tokens[self.idx].t {
                    self.idx += 1;
                    let parent_statemet = Stmt::Method(name.clone(), conditions.clone(), Rc::new(self.statement()?));
                    if let TokenData::ELSE = self.tokens[self.idx].t {
                        self.idx += 1;
                        if let TokenData::COLON = self.tokens[self.idx].t {
                            self.idx += 1;
                            if let TokenData::NEW_LINE = self.tokens[self.idx].t {
                                self.idx += 1;
                                let else_conditions = Expr::Unary(Token{line:0, col:0, t:TokenData::NOT}, Rc::new(conditions.unwrap()));
                                let else_statement = Stmt::Method(format!("Dont{}", name), Some(else_conditions), Rc::new(self.statement()?));
                                Ok(Stmt::Block(vec![parent_statemet, else_statement]))
                            } else {
                                let line = self.tokens[self.idx].line;
                                let col = self.tokens[self.idx].col;
                                Err(ParserError{line, col, message:String::from("Expected new line after ':'")})
                            }
                        } else {
                            let line = self.tokens[self.idx].line;
                            let col = self.tokens[self.idx].col;
                            return Err(ParserError{line, col, message:String::from("Expected ':' after method declaration.")})
                        }
                    } else {
                        Ok(parent_statemet)
                    }
                } else {
                    let line = self.tokens[self.idx].line;
                    let col = self.tokens[self.idx].col;
                    Err(ParserError{line, col, message:String::from("Expected new line after ':'")})
                }
            } else {
                let line = self.tokens[self.idx].line;
                let col = self.tokens[self.idx].col;
                return Err(ParserError{line, col, message:String::from("Expected ':' after method declaration.")})
            }
        } else {
            let line = self.tokens[self.idx].line;
            let col = self.tokens[self.idx].col;
            Err(ParserError{line, col, message:String::from("Expected method name.")})
        }
    }
    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if let TokenData::TASK = self.tokens[self.idx].t {
            self.idx += 1;
            self.task_statement()
        } else if let TokenData::BLOCK_START = self.tokens[self.idx].t {
            self.idx += 1;
            self.block_statement()
        } else if let TokenData::METHOD = self.tokens[self.idx].t {
            self.idx += 1;
            self.method_statement()
        } else if let TokenData::LABEL(_) = self.tokens[self.idx].t {
            if let TokenData::OPEN_PAREN = self.tokens[self.idx+1].t {
                self.call_statement()
            } else if let TokenData::EQUALS | TokenData::ADD_TO | TokenData::SUBTRACT_FROM | TokenData::MULTIPLY_BY | TokenData::DIVIDE_BY = self.tokens[self.idx+1].t {
                self.assignment_statement()
            } else {
                let line = self.tokens[self.idx].line;
                let col = self.tokens[self.idx].col;
                Err(ParserError{line, col, message:String::from("Expected function call or assignment.")})
            }
        } else {
            let line = self.tokens[self.idx].line;
            let col = self.tokens[self.idx].col;
            Err(ParserError{line, col, message:String::from("Expression statements are not supported.")})
            // self.expression_statement()
        }
    }
    pub fn parse(htn_source: &str) -> Result<Domain, ParserError> {
        let tokens = &Lexer::tokenize(htn_source)?;
        println!("{}", tokens.iter().fold(String::new(), |acc, item| acc + &format!("{}", item)));
        let mut parser = Parser{idx:0, tokens};
        loop {
            match parser.block_statement() {
                Err(e) => { println!("{}", e); parser.error_recover() },
                Ok(s) => { println!("{:?}", s); break}
            }
        }
        Ok(Domain{})
    }
}