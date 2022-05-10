use std::fmt;

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
    OPEN_PAREN,
    CLOSE_PAREN,
    COLON

}
pub struct Token {
    line: usize,
    col: usize,
    tab_depth: usize,
    t: TokenData,
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
        let mut tab_depth = 0;

        let mut depth_separator : Option<DepthSeparator> = None;
        let mut is_comment = false;
        let mut r = Vec::<Token>::new();
        for c in htn_source.chars() {
            if is_comment && c != '\n' {
                continue;
            }
            match c {
                '#' => is_comment = true,
                '\t' => if depth_separator.is_none() {
                        depth_separator = Some(DepthSeparator::TABS);
                    } else if let Some(DepthSeparator::TABS) = depth_separator {
                        // All good
                    } else {
                        return Err(ParserError{line, col, message:"File uses both tabs and spaces".to_string()});
                    }
                ':' => r.push(Token{line, col, tab_depth, t:TokenData::COLON}),
                '(' => r.push(Token{line, col, tab_depth, t:TokenData::OPEN_PAREN}),
                ')' => r.push(Token{line, col, tab_depth, t:TokenData::CLOSE_PAREN}),
                '\n' => { is_comment = false; line += 1; col = 1; tab_depth = 0; },
                c => {},
                
            }    
        }
        Ok(r)
    }
}

impl Parser {
    pub fn parse(htn_source: &str) -> Result<Domain, ParserError> {
        let tokens = Lexer::tokenize(htn_source)?;
        Ok(Domain{})
    }
}