use std::{fmt, string::ParseError};

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

pub enum TokenData {
    TASK,
    METHOD,
    ELSE,
    EFFECTS,
    LABEL(String),
    EQUALS,
    MINUS,
    OR,
    AND,
    NOT,
    OPEN_PAREN,
    CLOSE_PAREN,
    COLON,
    BLOCK_START,
    BLOCK_END,
    NEW_LINE

}
pub struct Token {
    line: usize,
    col: usize,
    // tab_depth: usize,
    t: TokenData,
}

impl fmt::Debug for TokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TASK => write!(f, "TASK"),
            Self::METHOD => write!(f, "METHOD"),
            Self::ELSE => write!(f, "ELSE"),
            Self::EFFECTS => write!(f, "EFFECTS"),
            Self::LABEL(arg0) => f.debug_tuple("LABEL").field(arg0).finish(),
            Self::OPEN_PAREN => write!(f, "OPEN_PAREN"),
            Self::CLOSE_PAREN => write!(f, "CLOSE_PAREN"),
            Self::COLON => write!(f, "COLON"),
            Self::BLOCK_START => write!(f, "BLOCK_START"),
            Self::BLOCK_END => write!(f, "BLOCK_END"),
            Self::NEW_LINE => write!(f, "NEW_LINE"),
            TokenData::EQUALS => write!(f, "EQUALS"),
            TokenData::MINUS => write!(f, "MINUS"),
            TokenData::OR => write!(f, "OR"),
            TokenData::AND => write!(f, "AND"),
            TokenData::NOT => write!(f, "NOT"),
        }
    }
}

impl fmt::Debug for Token{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Token").field("line", &self.line).field("col", &self.col).field("t", &self.t).finish()
    }
}

enum DepthSeparator {
    TABS,
    SPACES(usize)
}

pub struct Lexer {

}

pub struct Parser {

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
                c if c.is_alphanumeric() => {
                    if is_newline {
                        if let Some(DepthSeparator::SPACES(0)) = depth_separator {
                            depth_separator = Some(DepthSeparator::SPACES(tab_depth));
                        }
                        if let Some(DepthSeparator::SPACES(sc)) = depth_separator {
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
        }
        Ok(r)
    }
}

impl Parser {
    pub fn parse(htn_source: &str) -> Result<Domain, ParserError> {
        let tokens = Lexer::tokenize(htn_source)?;
        println!("{:?}", tokens);
        Ok(Domain{})
    }
}