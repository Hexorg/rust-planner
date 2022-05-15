use std::{fmt, rc::Rc};

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
    STATEMENT_END,
    EOF

}

#[derive(Clone, Debug)]
pub struct Token {
    line: usize,
    col: usize,
    len: usize,
    // tab_depth: usize,
    t: TokenData,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.t)
    }
}

impl fmt::Display for TokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenData::TASK => write!(f, "TASK"),
            TokenData::METHOD => write!(f, "METHOD"),
            TokenData::ELSE => write!(f, "ELSE"),
            TokenData::EFFECTS => write!(f, "EFFECTS"),
            TokenData::LABEL(l) => write!(f, "{}", l),
            TokenData::LITERAL(l) => write!(f, "'{}'", l),
            TokenData::EQUALS => write!(f, "="),
            TokenData::EQUALS_EQUALS => write!(f, "=="),
            TokenData::MINUS => write!(f, "-"),
            TokenData::PLUS => write!(f, "+"),
            TokenData::SLASH => write!(f, "/"),
            TokenData::STAR => write!(f, "*"),
            TokenData::GREATER => write!(f, ">"),
            TokenData::SMALLER => write!(f, "<task>"),
            TokenData::GREATER_OR_EQUALS => write!(f, ">="),
            TokenData::SMALLER_OR_EQUALS => write!(f, "<="),
            TokenData::NOT_EQUALS => write!(f, "!="),
            TokenData::SUBTRACT_FROM => write!(f, "-="),
            TokenData::ADD_TO => write!(f, "+="),
            TokenData::MULTIPLY_BY => write!(f, "*="),
            TokenData::DIVIDE_BY => write!(f, "/="),
            TokenData::OR => write!(f, "|"),
            TokenData::AND => write!(f, "&"),
            TokenData::NOT => write!(f, "!"),
            TokenData::OPEN_PAREN => write!(f, "("),
            TokenData::CLOSE_PAREN => write!(f, ")"),
            TokenData::COLON => write!(f, ":"),
            TokenData::BLOCK_START => write!(f, "{{"),
            TokenData::BLOCK_END => write!(f, "}}"),
            TokenData::STATEMENT_END => write!(f, ";"),
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

pub struct Parser {
    idx: usize,
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn tokenize(htn_source: &str) -> Result<Vec<Token>, ParserError> {
        let mut line = 1; // current source line, used for error reporting by Tokens
        let mut col = 1; // current source column, used for error reporting by Tokens
        let mut last_block_tab_depth = 0; // needed to insert BLOCK_END tokens properly
        let mut tab_depth = 0; // For tabs, tab_depth just counts tabs at the beginning of line 
        // For spaces, tab_depth counts spaces first, then on the first non-whitespace character figures out 
        // how many spaces are equal to a new block, and divides amount of spaces by however many spaces constitute a new block
        let mut depth_separator : Option<DepthSeparator> = None;
        let mut is_comment = false; // ignores the rest of the line
        let mut is_newline = true; // stays true between \n and first non-whitespace characters to allow for tabs vs spaces detection
        let mut r = Vec::<Token>::new(); // result
        let mut it = htn_source.chars().peekable();
        while let Some(c) = it.next() {
            if is_comment && c != '\n' {
                continue;
            }
            match c {
                '#' => is_comment = true,
                ':' => r.push(Token{line, col, len:1, t:TokenData::COLON}),
                '(' => r.push(Token{line, col, len:1, t:TokenData::OPEN_PAREN}),
                ')' => r.push(Token{line, col, len:1, t:TokenData::CLOSE_PAREN}),
                '=' => r.push(Token{line, col, len:1, t:TokenData::EQUALS}),
                '-' => r.push(Token{line, col, len:1, t:TokenData::MINUS}),
                '+' => r.push(Token{line, col, len:1, t:TokenData::PLUS}),
                '/' => r.push(Token{line, col, len:1, t:TokenData::SLASH}),
                '*' => r.push(Token{line, col, len:1, t:TokenData::STAR}),
                '>' => r.push(Token{line, col, len:1, t:TokenData::GREATER}),
                '<' => r.push(Token{line, col, len:1, t:TokenData::SMALLER}),
                '!' => r.push(Token{line, col, len:1, t:TokenData::NOT}),
                '|' => r.push(Token{line, col, len:1, t:TokenData::OR}),
                '&' => r.push(Token{line, col, len:1, t:TokenData::AND}),
                '\n' => { 
                    is_comment = false; 
                    is_newline = true;
                    tab_depth = 0;
                    line += 1; 
                    col = 1; 
                },
                c if !c.is_whitespace() => {
                    if is_newline { 
                        // Now figure out tabs vs spaces and what tab_depth of current line is
                        // for block start/end detection
                        if let Some(DepthSeparator::SPACES(0)) = depth_separator { // we've seen spaces at the beginning of the line before, but it's the first ever word that's offset
                            depth_separator = Some(DepthSeparator::SPACES(tab_depth));
                            tab_depth = 1;
                        } else if let Some(DepthSeparator::SPACES(sc)) = depth_separator { // we already know that we're using spaces and how many
                            if tab_depth % sc != 0 {
                                return Err(ParserError{line, col, message:String::from("Unexpected amount of spaces.")});
                            }
                            tab_depth = tab_depth / sc;
                        } // else we're using tabs and '\t' => branch of this match counts tab_depth properly
                        if tab_depth > last_block_tab_depth { // new block start
                            if let Some(Token{t:TokenData::BLOCK_END,..}) = r.last() {
                                return Err(ParserError{line, col, message:String::from("Unexpected block identation.")});
                            }
                            r.push(Token{line, col:1, len:0, t:TokenData::BLOCK_START});
                            last_block_tab_depth = tab_depth;
                        } else if tab_depth == last_block_tab_depth && r.len() > 0 { // it's a new line for the old block. Previous statement has ended.
                            r.push(Token{line:line-1, col:r.last().unwrap().col+1, len:0, t:TokenData::STATEMENT_END});
                        } else {
                            while tab_depth < last_block_tab_depth {  // block(s) have ended
                                if let Some(Token{t:TokenData::BLOCK_END,..}) = r.last() {
                                    // if multiple blocks are ending, just keep adding BLOCK_END
                                } else {
                                    // last block has ended, so did the previous statement
                                    r.push(Token{line:line-1, col:r.last().unwrap().col+1, len:0, t:TokenData::STATEMENT_END});
                                }
                                r.push(Token{line, col:1, len:0, t:TokenData::BLOCK_END});
                                last_block_tab_depth -=1;
                            }
                        }
                        is_newline = false;
                    } // done with tab depth detection
                    // Now to see what kind of token this character will yield.
                    if let Some(mut last_token) = r.last_mut() {
                        if let Token{t:TokenData::LABEL(label), ..} = last_token {
                            label.push(c);
                            last_token.len += 1;
                            if let Some(t) = if let Some(next_char) = it.peek() {
                                if !next_char.is_alphanumeric() {
                                    match label.clone().as_str() { // if the label composes a keyword, replace with keyword
                                        "task" => Some(TokenData::TASK),
                                        "method" => Some(TokenData::METHOD),
                                        "else" => Some(TokenData::ELSE),
                                        "effects" => Some(TokenData::EFFECTS),
                                        "or" => Some(TokenData::OR),
                                        "and" => Some(TokenData::AND),
                                        "not" => Some(TokenData::NOT),
                                        "false" => Some(TokenData::LITERAL(0)),
                                        "true" => Some(TokenData::LITERAL(1)),
                                        _ => if let Ok(literal) = label.parse::<i32>() {
                                                Some(TokenData::LITERAL(literal))
                                            } else { None },
                                    }
                                } else { None }
                            } else { None } {
                                last_token.t = t;
                            }
                        } else { // last token isn't a label so we're starting a new label or keyword
                            r.push(Token{line, col, len:1, t:TokenData::LABEL(String::from(c))})
                        }
                        
                        
                    } else {
                        r.push(Token{line, col, len:1, t:TokenData::LABEL(String::from(c))})
                    }
                },
                '\t' if is_newline => { if depth_separator.is_none() { // if we don't know if we're using tabs or spaces
                        depth_separator = Some(DepthSeparator::TABS);
                    } else if let Some(DepthSeparator::SPACES(_)) = depth_separator {
                        return Err(ParserError{line, col, message:String::from("Tabs and spaces can't be used together")})
                    }
                    tab_depth += 1;
                },
                c if c.is_whitespace() && is_newline => { 
                        if depth_separator.is_none() {
                            depth_separator = Some(DepthSeparator::SPACES(0));
                        } else if let Some(DepthSeparator::TABS) = depth_separator {
                            return Err(ParserError{line, col, message:String::from("Tabs and spaces can't be used together")})
                        }
                        tab_depth += 1; // tab_depth counts amount of spaces seen first. Then on the first non-whitespace character we convert amount of spaces to actual tab-depth
                },
                _ => (),
            }
            col += 1;
            if let Some(Token{t:TokenData::EQUALS,..}) = r.last() { // combine multi-symbol operands like -= and <=
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
        }
        // End of file. Finish off whatever blocks have been here
        // if last_block_tab_depth > 0 {
        if r.len() > 0 {
            r.push(Token{line, col, len:0, t:TokenData::STATEMENT_END});
        }
        while last_block_tab_depth > 0 {
            r.push(Token{line, col, len:0, t:TokenData::BLOCK_END});
            last_block_tab_depth -= 1;
            }
        // }
        r.push(Token{line, col, len:0, t:TokenData::EOF});
        Ok(r)
    }
}
#[derive(Clone, Debug)]
pub enum Expr {
    Binary(Rc<Expr>, Token, Rc<Expr>), // left Token right
    Grouping(Rc<Expr>), // group of expressions
    Literal(i32),
    Variable(String),
    Unary(Token, Rc<Expr>), // Token right
    Assignment(String, Rc<Expr>), // name, value
    Call(Rc<Expr>, Token, Vec<Expr>) // callee expr, closing parenthesis token, args
}

#[derive(Debug)]
pub enum Stmt {
    Method(String, Option<Expr>, Rc<Stmt>), // Name, condition, subtasks
    Task(String, Option<Expr>, Rc<Stmt>, Option<Rc<Stmt>>), // Name, condition, methods, effects
    Block(Vec<Stmt>),
    Expression(Expr)
}

macro_rules! pexpect {
    ($s:expr, $p:pat, $do:block, $e:literal) => {
        // if $s.idx + 1 < $s.tokens.len() {
            if let $p = &$s.tokens[$s.idx].t {
                $s.idx += 1;
                $do
            } else {
                let line = $s.tokens[$s.idx].line;
                let col = $s.tokens[$s.idx].col;
                Err(ParserError{line, col, message:String::from($e)})
            }
        // } else {
        //     let line = $s.tokens[$s.tokens.len()-1].line;
        //     let col = $s.tokens[$s.tokens.len()-1].col + $s.tokens[$s.tokens.len()-1].len;
        //     Err(ParserError{line, col, message:String::from("Unexpected end of file")})
        // }
    }
}

macro_rules! pmatch {
    ($s:expr, $p:pat, $do:block) => {
        if let $p = &$s.tokens[$s.idx].t {
            $s.idx += 1;
            $do
        } 
    }
}

macro_rules! ptest {
    ($s:expr, $p:pat, $do_match:block else $do_nomatch:block) => {
            if let $p = &$s.tokens[$s.idx].t {
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
            if let TokenData::STATEMENT_END = self.tokens[self.idx-1].t {
                return;
            }
            if let TokenData::TASK | TokenData::METHOD = self.tokens[self.idx].t {
                return;
            }
            self.idx += 1;
        }
    }
    fn call(&mut self) -> Result<Expr, ParserError> {
        let expr = self.primary()?;
        ptest!(self, TokenData::OPEN_PAREN, {
            let mut args = Vec::<Expr>::new();
            loop { 
                ptest!(self, TokenData::CLOSE_PAREN, {
                    break Ok(Expr::Call(Rc::new(expr), self.tokens[self.idx-1].clone(), args))
                } else {
                    args.push(self.expression()?);
                })
            }
        } else {
            Ok(expr)
        })

    }
    fn unary(&mut self) -> Result<Expr, ParserError> {
        ptest!(self, (TokenData::NOT | TokenData::MINUS), {
            let operator = self.tokens[self.idx-1].clone();
            let right = self.unary()?;
            Ok(Expr::Unary(operator, Rc::new(right)))
        } else {
            self.call()
        }) 
    }
    fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;
        while let TokenData::SLASH | TokenData::STAR | TokenData::AND = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.unary()?;
            expr = Expr::Binary(Rc::new(expr), operator, Rc::new(right));
        }
        Ok(expr)
    }
    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;
        while let TokenData::MINUS | TokenData::PLUS | TokenData::OR = self.tokens[self.idx].t {
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
    fn assignment(&mut self) -> Result<Expr, ParserError> {
        let target = self.equality()?;
        ptest!(self, (TokenData::EQUALS 
            | TokenData::ADD_TO 
            | TokenData::SUBTRACT_FROM
            | TokenData::DIVIDE_BY
            | TokenData::MULTIPLY_BY), {
                if let Expr::Variable(varname) = &target {
                    let value_expr = match self.tokens[self.idx].t {
                        // TokenData::EQUALS => self.expression()?,
                        TokenData::ADD_TO => Expr::Binary(Rc::new(Expr::Variable(varname.clone())), Token{line:0, col:0, len:0, t:TokenData::PLUS}, Rc::new(self.expression()?)),
                        TokenData::SUBTRACT_FROM => Expr::Binary(Rc::new(Expr::Variable(varname.clone())), Token{line:0, col:0, len:0, t:TokenData::MINUS}, Rc::new(self.expression()?)),
                        TokenData::MULTIPLY_BY => Expr::Binary(Rc::new(Expr::Variable(varname.clone())), Token{line:0, col:0, len:0, t:TokenData::STAR}, Rc::new(self.expression()?)),
                        TokenData::DIVIDE_BY => Expr::Binary(Rc::new(Expr::Variable(varname.clone())), Token{line:0, col:0, len:0, t:TokenData::SLASH}, Rc::new(self.expression()?)),
                        _ => self.expression()?,
                    };
                    Ok(Expr::Assignment(varname.clone(), Rc::new(value_expr)))
                } else {
                    let line = self.tokens[self.idx].line;
                    let col = self.tokens[self.idx].col;
                    Err(ParserError{line, col, message:String::from("Invalid assignment target.")})
                }
        } else {
            Ok(target)
        })
    }
    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.assignment()
    }
    fn primary(&mut self) -> Result<Expr, ParserError> {
        // Literal | "(" expression ")"
        if let TokenData::LITERAL(val)  = self.tokens[self.idx].t {
            self.idx += 1;
            Ok(Expr::Literal(val))
        } else if let TokenData::OPEN_PAREN = self.tokens[self.idx].t {
            self.idx += 1;
            let expr = self.expression()?;
            pexpect!(self, TokenData::CLOSE_PAREN, {
                Ok(Expr::Grouping(Rc::new(expr)))
            }, "Expected ')' after expression.")
        } else if let TokenData::LABEL(name) = &self.tokens[self.idx].t {
            self.idx += 1;
            Ok(Expr::Variable(name.clone()))
        } else {
            let line = self.tokens[self.idx].line;
            let col = self.tokens[self.idx].col;
            Err(ParserError{line, col, message:String::from("Expected expression.")})
        }

    }
    fn effects_statement(&mut self) -> Result<Stmt, ParserError> {
        pexpect!(self, TokenData::COLON, {self.statement()}, "Expected ':' after 'effects'.")
    }
    fn task_statement(&mut self) -> Result<Stmt, ParserError> {
        pexpect!(self, TokenData::LABEL(name), {
            let name = name.clone();
            let mut conditions : Option<Expr> = None;
            pmatch!(self, TokenData::OPEN_PAREN, {
                conditions = Some(self.expression()?);
                pexpect!(self, TokenData::CLOSE_PAREN, {Ok(())}, "Expected ')' after task conditions.")?
            });
            pexpect!(self, TokenData::COLON, {
                let task_block = self.statement()?;
                let mut effects_block = None;
                pmatch!(self, TokenData::EFFECTS, {effects_block = Some(Rc::new(self.effects_statement()?));});
                Ok(Stmt::Task(name, conditions, Rc::new(task_block), effects_block))
            }, "Expected ':' after task label.")
        }, "Expected label after 'task'.")
        
    }
    fn expression_statement(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;
        pexpect!(self, 
            (TokenData::STATEMENT_END | TokenData::BLOCK_START | TokenData::BLOCK_END), 
            {Ok(Stmt::Expression(expr))}, 
            "Expected new line after expression.")
    }
    fn block_statement(&mut self) -> Result<Stmt, ParserError> {
        let mut stmts = Vec::<Stmt>::new();
        loop {
            let mut stmt = self.statement()?;
            if let Stmt::Block(ref mut inner) = stmt {
                stmts.append(inner);
            } else {
                stmts.push(stmt);
            }

            if let TokenData::BLOCK_END | TokenData::EOF = self.tokens[self.idx].t {
                self.idx += 1;
                return Ok(Stmt::Block(stmts))
            } 
        }
        
    }
    fn method_statement(&mut self) -> Result<Stmt, ParserError> {
        pexpect!(self, TokenData::LABEL(name), {
            let name = name.clone();
            let mut conditions: Option<Expr> = None;
            pmatch!(self, TokenData::OPEN_PAREN, {
                conditions = Some(self.expression()?);
                pexpect!(self, TokenData::CLOSE_PAREN, {Ok(())}, "Expected ')' after method conditions.'")?;
            });
            pexpect!(self, TokenData::COLON, {
                let parent_statemet = Stmt::Method(name.clone(), conditions.clone(), Rc::new(self.statement()?));
                ptest!(self, TokenData::ELSE, {
                    pexpect!(self, TokenData::COLON, {
                        let else_conditions = Expr::Unary(Token{line:0, col:0, len:0, t:TokenData::NOT}, Rc::new(conditions.unwrap()));
                        let else_statement = Stmt::Method(format!("Dont{}", name), Some(else_conditions), Rc::new(self.statement()?));
                        Ok(Stmt::Block(vec![parent_statemet, else_statement]))
                    }, "Expected ':' after else clause.")
                } else {
                    Ok(parent_statemet)
                })
            }, "Expected ':' after method declaration.")
        }, "Expected method name.")
    }
    fn statement(&mut self) -> Result<Stmt, ParserError> {
        // println!("When Parsing a new statement, next token is {}.", self.tokens[self.idx]);
        let r = if let TokenData::TASK = self.tokens[self.idx].t {
            self.idx += 1;
            self.task_statement()
        } else if let TokenData::BLOCK_START = self.tokens[self.idx].t {
            self.idx += 1;
            self.block_statement()
        } else if let TokenData::METHOD = self.tokens[self.idx].t {
            self.idx += 1;
            self.method_statement()
        } else if let TokenData::EOF = self.tokens[self.idx].t {
            let line = self.tokens[self.idx].line;
            let col = self.tokens[self.idx].col;
            Err(ParserError{line, col, message:String::from("Reached the end of file")})
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
                    TokenData::BLOCK_START => {depth += 1; format!("{}", item)},
                    TokenData::BLOCK_END => {format!("{}\n", item)},
                    TokenData::COLON => format!("{}\n", item),
                    _ => format!("{} ", item),
                };
                let r = if acc.ends_with('\n') || acc.len() == 0 {
                    acc + &format!("{:4}: {:width$}{}", item.line, ' ', item_string, width=depth*4)
                } else if acc.ends_with('{') {
                    acc + &format!(" {}", item_string)
                } else {
                    acc + &format!("{}", item_string)
                };
                if let TokenData::BLOCK_END = item.t {
                    depth -= 1;
                };
                r
            }
        ));
    }
    pub fn print_parse_errors(errors: Vec<ParserError>, htn_source:&str, filepath:&str) {
        let mut lines = htn_source.lines();
        let mut last_error_line = 0;
        for e in errors {
            if let Some(eline) = lines.nth(e.line - last_error_line-1) {
                let line_number_string = format!("{}", e.line);
                eprintln!("{}:{} Error:", filepath, e.line); 
                eprintln!("\t{}: {}", line_number_string, eline);
                last_error_line = e.line;
                let debug_str_col_pos = line_number_string.len() + 2 + e.col;
                eprintln!("\t{:->width$} {}\n",'^', e.message, width=debug_str_col_pos); 
            }
        }
    }
    pub fn parse(htn_source: &str) -> (Vec<Stmt>, Vec<ParserError>) {
        let mut ast = Vec::<Stmt>::new();
        let mut errors = Vec::<ParserError>::new();
        let mut parser = match Lexer::tokenize(htn_source) {
            Ok(tokens) => Parser{idx:0, tokens},
            Err(e) => { errors.push(e); return (ast, errors); }
        };
        Parser::print_tokens(&parser.tokens);
        while parser.idx + 1 < parser.tokens.len() {
            match parser.statement() {
                Err(e) => {errors.push(e); parser.error_recover(); },
                Ok(s) => ast.push(s)
            }
        }
        (ast, errors)
    }
}