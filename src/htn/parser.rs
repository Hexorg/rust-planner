use std::{fmt, string::ParseError, rc::Rc};

use super::domain::Domain;

pub struct ParserError {
    line: usize,
    col: usize,
    message: String
}

impl fmt::Debug for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ParserError")
    }
}

#[derive(Debug, Clone)]
pub enum TokenData {
    TASK,
    METHOD,
    ELSE,
    EFFECTS,
    LABEL(String),
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

#[derive(Clone)]
pub struct Token {
    line: usize,
    col: usize,
    // tab_depth: usize,
    t: TokenData,
}

impl fmt::Debug for Token{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Token").field("line", &self.line).field("col", &self.col).field("t", &self.t).finish()
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
            TokenData::NEW_LINE => write!(f, "\n"),
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
                    println!("Double new line at line:{} col:{} last_line_tab_depth:{}", line, col, last_line_tab_depth);
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
                        } else if let Some(DepthSeparator::SPACES(sc)) = depth_separator {
                            if tab_depth % sc != 0 {
                                return Err(ParserError{line, col, message:String::from("Unexpected amount of spaces")});
                            }
                            tab_depth = tab_depth / sc;
                        }
                        if tab_depth > last_line_tab_depth {
                            r.push(Token{line, col, t:TokenData::BLOCK_START});
                        } else if tab_depth < last_line_tab_depth {
                            r.push(Token{line, col, t:TokenData::BLOCK_END});
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

        }
        r.push(Token{line, col, t:TokenData::EOF});
        Ok(r)
    }
}
enum AST {
    Binary(Rc<AST>, Token, Rc<AST>),
    Grouping(Rc<AST>),
    Literal(Token),
    Unary(Token, Rc<AST>)
}

impl fmt::Debug for AST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binary(arg0, arg1, arg2) => write!(f, "({:?} {} {:?})", arg0, arg1.t, arg2),
            Self::Grouping(arg0) => f.debug_tuple("Grouping").field(arg0).finish(),
            Self::Literal(arg0) => write!(f, "{}", arg0.t),
            Self::Unary(arg0, arg1) => f.debug_tuple("Unary").field(arg0).field(arg1).finish(),
        }
    }
}

impl Parser<'_> {
    fn unary(&mut self) -> Result<AST, ParserError> {
        if let TokenData::NOT | TokenData::MINUS = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.unary()?;
            Ok(AST::Unary(operator, Rc::new(right)))
        } else {
            self.primary()
        }   
    }
    fn factor(&mut self) -> Result<AST, ParserError> {
        let mut ast = self.unary()?;
        while let TokenData::SLASH | TokenData::STAR = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.unary()?;
            ast = AST::Binary(Rc::new(ast), operator, Rc::new(right));
        }
        Ok(ast)
    }
    fn term(&mut self) -> Result<AST, ParserError> {
        let mut ast = self.factor()?;
        while let TokenData::MINUS | TokenData::PLUS = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.factor()?;
            ast = AST::Binary(Rc::new(ast), operator, Rc::new(right));
        }
        Ok(ast)
    }
    fn comparison(&mut self) -> Result<AST, ParserError> {
        let mut ast = self.term()?;
        while let TokenData::GREATER | TokenData::GREATER_OR_EQUALS | TokenData::SMALLER | TokenData::SMALLER_OR_EQUALS = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.term()?;
            ast = AST::Binary(Rc::new(ast), operator, Rc::new(right));
        }
        Ok(ast)
    }
    fn equality(&mut self) -> Result<AST, ParserError> {
        let mut ast = self.comparison()?;
        while let TokenData::NOT_EQUALS | TokenData::EQUALS_EQUALS = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.comparison()?;
            ast = AST::Binary(Rc::new(ast), operator, Rc::new(right));
        }
        Ok(ast)
    }
    fn expression(&mut self) -> Result<AST, ParserError> {
        self.equality()
    }
    fn primary(&mut self) -> Result<AST, ParserError> {
        // Literal | "(" expression ")"
        if let TokenData::LABEL(_) = self.tokens[self.idx].t {
            let token = self.tokens[self.idx].clone();
            self.idx += 1;
            Ok(AST::Literal(token))
        } else if let TokenData::OPEN_PAREN = self.tokens[self.idx].t {
            let ast = self.expression()?;
            if let TokenData::CLOSE_PAREN = self.tokens[self.idx].t {
                self.idx += 1;
                Ok(ast)
            } else {
                let line = self.tokens[self.idx].line;
                let col = self.tokens[self.idx].col;
                Err(ParserError{line, col, message:String::from("Expected ')' after expression.")})
            }
        } else {
            let line = self.tokens[self.idx].line;
            let col = self.tokens[self.idx].col;
            Err(ParserError{line, col, message:String::from("Unexpected end of primary expression.")})
        }

    }
    pub fn parse(htn_source: &str) -> Result<Domain, ParserError> {
        let tokens = &Lexer::tokenize(htn_source)?;
        println!("{:?}", tokens);
        let mut parser = Parser{idx:0, tokens};
        println!("{:?}", parser.expression()?);
        Ok(Domain{})
    }
}