use std::iter::Peekable;
use std::str::CharIndices;

use super::tokens::{Token, TokenData::{self, *}, Literal};
use super::Error;

#[derive(Debug, PartialEq)]
enum DepthSeparator {
    TABS,
    SPACES(usize)
}

pub struct Lexer<'a> { 
    text:&'a str,
    it: Peekable<CharIndices<'a>>,
    line: usize, // current source line, used for error reporting by Tokens
    col: usize, // current source column, used for error reporting by Tokens
    stash: Vec<Token<'a>>, // stash of tokens to return in leu of it.peek_two()
    is_newline: bool,
    tab_depth: usize, // needed to insert BLOCK_END tokens properly
    depth_separator: Option<DepthSeparator>,
}

impl<'a> Lexer<'a> {
    pub fn new(text:&'a str) -> Self {
        Self{
            text,
            it:text.char_indices().peekable(), 
            line:1,
            col:1,
            stash:Vec::new(),
            is_newline: true,
            tab_depth: 0,
            depth_separator: None,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.stash.len() > 0 {
            Some(Ok(self.stash.pop().unwrap()))
        } else {
            if let Some((offset, c)) = self.next_char() {
                // println!("Consumed up to: line:{} col:{} offset:{} char:{:?}", self.line, self.col, offset, c);
                let mut new_token = match c {
                    ':' => Some(Ok(Token{line:self.line, col:self.col, len:1, t:Colon})),
                    '(' => Some(Ok(Token{line:self.line, col:self.col, len:1, t:OpenParenthesis})),
                    ')' => Some(Ok(Token{line:self.line, col:self.col, len:1, t:CloseParenthesis})),
                    ',' => Some(Ok(Token{line:self.line, col:self.col, len:1, t:Comma})),
                    '|' => Some(Ok(Token{line:self.line, col:self.col, len:1, t:Or})),
                    '&' => Some(Ok(Token{line:self.line, col:self.col, len:1, t:And})),
                    '.' => Some(Ok(Token{line:self.line, col:self.col, len:1, t:Dot})),
                    '=' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{line:self.line, col:self.col, len:2, t:EqualsEquals}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{line:self.line, col:self.col, len:1, t:Equals})),
                    },
                    '-' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{line:self.line, col:self.col, len:2, t:SubtractFrom}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{line:self.line, col:self.col, len:1, t:Minus})),
                    },
                    '+' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{line:self.line, col:self.col, len:2, t:AddTo}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{line:self.line, col:self.col, len:1, t:Plus})),
                    },
                    '/' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{line:self.line, col:self.col, len:2, t:DivideBy}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{line:self.line, col:self.col, len:1, t:Slash})),
                    },
                    '*' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{line:self.line, col:self.col, len:2, t:MultiplyBy}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{line:self.line, col:self.col, len:1, t:Star})),
                    },
                    '>' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{line:self.line, col:self.col, len:2, t:GreaterOrEquals}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{line:self.line, col:self.col, len:1, t:Greater})),
                    },
                    '<' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{line:self.line, col:self.col, len:2, t:SmallerOrEquals}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{line:self.line, col:self.col, len:1, t:Smaller})),
                    },
                    '!' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{line:self.line, col:self.col, len:2, t:NotEquals}; self.it.next(); Some(Ok(t))},
                        _ => Some(Ok(Token{line:self.line, col:self.col, len:1, t:Not}))
                    },
                    '"' => {Some(self.string(offset))},
                    c if c.is_whitespace() => if self.is_newline { 
                            self.is_newline = false;
                            if let Some(e) = self.block(c) { 
                                Some(Err(e))
                            } else { 
                                // self block will push on the stack if they are needed.
                                // pull either the needed block start or the next token
                                // and pass it down
                                return self.next() // need to return because self.next() already added next token's length to self.col
                            }  
                        } else { 
                            // whitespace in the middle of a line. It's not a token, 
                            // So self.col += token.len() won't count. Therefore col should be incremented
                            self.col += 1;
                            return self.next() // need to return because self.next() already added next token's length to self.col
                        },
                    c if (c.is_alphabetic() || c == '_') => Some(self.identifier(offset)),
                    c if c.is_digit(10) => Some(self.number(offset)),
                    _ => {let e = Some(Err(Error{line:self.line, col:self.col, message:"Unexpected character.".to_owned()})); e}
                };
                if let Some(result) = &new_token {
                    match result {
                        Ok(t) => self.col += t.len,
                        Err(_) => self.col += 1,
                    }
                }
                if let Some((_, '\n')) | None  = self.it.peek() {
                    if let Some(Token{t:StatementEnd,..}) = self.stash.last() { } else {
                        self.stash.push(Token{line:self.line, col:self.col, len:0, t:StatementEnd});
                    }
                }
                if let Some(Ok(token)) = new_token {
                    if self.is_newline && self.tab_depth > 0 {
                        match token { 
                            Token{t:BlockStart,..} => (),
                            _ => {
                                self.stash.push(token); 
                                // self.col -= token.len();
                                self.tab_depth-=1; 
                                new_token = Some(Ok(Token{line:self.line, col:0, len:0, t:BlockEnd}));
                                while self.tab_depth > 0 {
                                    self.stash.push(Token{line:self.line, col:0, len:0, t:BlockEnd});
                                    self.tab_depth -= 1;
                                }
                            }
                        }
                    }
                }
                self.is_newline = false;

                new_token
            } else {
                if self.tab_depth > 0 {
                    self.tab_depth -= 1;
                    Some(Ok(Token{line:self.line, col:self.col, len:0, t:BlockEnd}))
                } else {
                    None
                }
            }
        }
    }
}

impl<'a> Lexer<'a> {
    fn next_char(&mut self) -> Option<(usize, char)> {
        let c = self.it.next();
        match c {
            Some((_, '\n')) => {self.col = 1; self.line += 1; self.is_newline = true; self.next_char()},
            Some((_, '#')) => {while let Some(_) = self.it.next_if(|(_,c)| *c != '\n') {}; self.next_char()},
            _ => c,
        }
    }

    fn string(&mut self, offset:usize) -> Result<Token<'a>, Error> {
        self.col += 1; // increment col for the first '"' since self.col += token.len() will only count the string itself
        let mut len = 0;
        let offset = offset + '"'.len_utf8();
        while let Some(_) = self.it.next_if(|(_,c)| *c != '"' ) { len += 1; }
        let slice = if let Some((string_end, _)) = self.it.next() { &self.text[offset..string_end] } else { &self.text[offset..]};
        let r = Ok(Token{line:self.line, col:self.col, len, t:Literal(Literal::S(slice)), });
        self.col += 1; // count the last '"'
        r
    }

    fn number(&mut self, offset:usize) -> Result<Token<'a>, Error> {
        let mut contains_dot = false;
        let mut len = 1;
        while let Some((_,c)) = self.it.next_if(|(_,c)| c.is_digit(10) || *c == '.') { len += 1; if c == '.' { contains_dot = true; }}
        let slice = if let Some((number_end, _)) = self.it.peek() { &self.text[offset..*number_end]} else { &self.text[offset..]};
        if contains_dot {
            match slice.parse::<f32>() {
                Ok(literal) => Ok(Token{line:self.line, col:self.col, len, t:Literal(Literal::F(literal))}),
                Err(_) => Err(Error{line:self.line, col:self.col, message:"Unable to parse float.".to_string()})
            }
        } else {
            match slice.parse::<i32>() {
                Ok(literal) => Ok(Token{line:self.line, col:self.col, len, t:Literal(Literal::I(literal))}),
                Err(_) => Err(Error{line:self.line, col:self.col, message:"Unable to parse integer.".to_string()})
            }
        }
    }

    fn identifier(&mut self, offset:usize) -> Result<Token<'a>, Error> {
        let mut len = 1;
        while let Some(_) = self.it.next_if(|(_,c)| (c.is_alphanumeric() || *c == '_')) { len += 1; }
        let slice = if let Some((identifier_end,_)) = self.it.peek() { &self.text[offset..*identifier_end]} else { &self.text[offset..]};
        let token = match slice {
            "task" => Ok(Token{line:self.line, col:self.col, len, t:Task}),
            "planning" => Ok(Token{line:self.line, col:self.col, len, t:Planning}),
            "else" => Ok(Token{line:self.line, col:self.col, len, t:Else}),
            "effects" => Ok(Token{line:self.line, col:self.col, len, t:Effects}),
            "include" => Ok(Token{line:self.line, col:self.col, len, t:Include}),
            "pass" => Ok(Token{line:self.line, col:self.col, len, t:Pass}),
            "cost" => Ok(Token{line:self.line, col:self.col, len, t:Cost}),
            "or" => Ok(Token{line:self.line, col:self.col, len, t:Or}),
            "and" => Ok(Token{line:self.line, col:self.col, len, t:And}),
            "not" => Ok(Token{line:self.line, col:self.col, len, t:Not}),
            "for" => Ok(Token{line:self.line, col:self.col, len, t:For}),
            "as" => Ok(Token{line:self.line, col:self.col, len, t:As}),
            "type" => Ok(Token{line:self.line, col:self.col, len, t:Type}),
            "false" => Ok(Token{line:self.line, col:self.col, len, t:Literal(Literal::B(false))}),
            "true" => Ok(Token{line:self.line, col:self.col, len, t:Literal(Literal::B(true))}),
            _ => Ok(Token{line:self.line, col:self.col, len, t:Identifier(slice)})
        };
        token
    }

    fn block(&mut self, c:char) -> Option<Error> {
        let mut len = 1;
        let mut is_tabs = c == '\t';
        let mut is_spaces = c == ' ';
        while let Some((_,c)) = self.it.next_if(|(_,c)| c.is_whitespace() && *c != '\n') { len += 1; match c { 
            ' ' => is_spaces = true,
            '\t' => is_tabs = true,
            _ => (),
        } }
        if let Some((_,'\n')) | Some((_,'#')) | None = self.it.peek() { None } else {
            match (is_tabs, is_spaces) {
                (true, false) => self.tabs(len),
                (false, true) => self.spaces(len),
                (true, true) |
                (false, false) => Some(Error{line:self.line, col:self.col, message:"Only tabs or spaces are allowed at the beginning of the line.".to_owned()}),
            }
        }
    }

    fn tabs(&mut self, count:usize) -> Option<Error> {
        if self.depth_separator.is_none() {
            self.depth_separator = Some(DepthSeparator::TABS);
        }
        if let Some(DepthSeparator::TABS) = self.depth_separator {
            if self.tab_depth <= count {
                self.col += self.tab_depth;
            } else if self.tab_depth > count {
                self.col += count;
            }
            while self.tab_depth < count {
                self.stash.push(Token{line:self.line, col:self.col, len:1, t:BlockStart});
                self.col += 1;
                self.tab_depth += 1;
            }
            while self.tab_depth > count {
                self.stash.push(Token{line:self.line, col:0, len:0, t:BlockEnd});
                self.tab_depth -= 1;
            }
            return None
        }
        return Some(Error{line:self.line, col:self.col, message:"Expected spaces at the beginning of the line.".to_owned()})
    }

    fn spaces(&mut self, count:usize) -> Option<Error> {
        if self.depth_separator.is_none() {
            self.depth_separator = Some(DepthSeparator::SPACES(count));
        }
        if let Some(DepthSeparator::SPACES(one_depth)) = self.depth_separator {
            if count % one_depth != 0 {
                return Some(Error{line:self.line, col:self.col, message:format!("Extra spaces. Expected {}.", one_depth)})
            }
            let count = count / one_depth;
            if self.tab_depth <= count {
                self.col += self.tab_depth*one_depth;
            } else if self.tab_depth > count {
                self.col += count*one_depth;
            }
            while self.tab_depth < count {
                self.stash.push(Token{line:self.line, col:self.col, len:one_depth, t:BlockStart});
                self.col += one_depth;
                self.tab_depth += 1;
            }
            while self.tab_depth > count {
                self.stash.push(Token{line:self.line, col:0, len:0, t:BlockEnd});
                self.tab_depth -= 1;
            }
            return None
        }
        return Some(Error{line:self.line, col:self.col, message:"Expected tabs at the beginning of the line.".to_owned()})
    }
}

#[cfg(test)]
mod tests {
    use crate::htn::parser::{lexer::DepthSeparator, Error};

    use super::{Lexer, Token, TokenData::*, Literal::*};
    #[test]
    fn test_include() {
        let code = "include \"str\"";
        let mut l = Lexer::new(code);
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:1, len:7, t:Include})));
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:10, len:3, t:Literal(S("str"))})));
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:14, len:0, t:StatementEnd})));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_comments() {
        let code = "# haha\n  # haha\n\ntask";
        let mut l = Lexer::new(code);
        assert_eq!(l.next(), Some(Ok(Token{line:4, col:1, len:4, t:Task})));
        assert_eq!(l.next(), Some(Ok(Token{line:4, col:5, len:0, t:StatementEnd})));
        assert_eq!(l.next(), None);
        assert_eq!(l.depth_separator, None);
    }

    #[test]
    fn test_space_block() {
        let code = "  &\n    &\n  &\n&";
        let mut l = Lexer::new(code);
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:1, len:2, t:BlockStart})));
        assert_eq!(l.depth_separator, Some(DepthSeparator::SPACES(2)));
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:3, len:1, t:And})));
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:4, len:0, t:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:2, col:3, len:2, t:BlockStart})));
        assert_eq!(l.next(), Some(Ok(Token{line:2, col:5, len:1, t:And})));
        assert_eq!(l.next(), Some(Ok(Token{line:2, col:6, len:0, t:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:3, col:0, len:0, t:BlockEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:3, col:3, len:1, t:And})));
        assert_eq!(l.next(), Some(Ok(Token{line:3, col:4, len:0, t:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:4, col:0, len:0, t:BlockEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:4, col:1, len:1, t:And})));
        assert_eq!(l.next(), Some(Ok(Token{line:4, col:2, len:0, t:StatementEnd})));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_tab_block() {
        let code = "\t&\n\t\t&\n\t&\n&";
        let mut l = Lexer::new(code);
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:1, len:1, t:BlockStart})));
        assert_eq!(l.depth_separator, Some(DepthSeparator::TABS));
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:2, len:1, t:And})));
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:3, len:0, t:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:2, col:2, len:1, t:BlockStart})));
        assert_eq!(l.next(), Some(Ok(Token{line:2, col:3, len:1, t:And})));
        assert_eq!(l.next(), Some(Ok(Token{line:2, col:4, len:0, t:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:3, col:0, len:0, t:BlockEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:3, col:2, len:1, t:And})));
        assert_eq!(l.next(), Some(Ok(Token{line:3, col:3, len:0, t:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:4, col:0, len:0, t:BlockEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:4, col:1, len:1, t:And})));
        assert_eq!(l.next(), Some(Ok(Token{line:4, col:2, len:0, t:StatementEnd})));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_error_block() {
        let code = " &\n\t&";
        let mut l = Lexer::new(code);
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:1, len:1, t:BlockStart})));
        assert_eq!(l.depth_separator, Some(DepthSeparator::SPACES(1)));
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:2, len:1, t:And})));
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:3, len:0, t:StatementEnd})));
        assert_eq!(l.next(), Some(Err(Error{line:2, col:1, message:"Expected spaces at the beginning of the line.".to_owned()})));
        assert_eq!(l.next(), Some(Ok(Token{line:2, col:2, len:1, t:And})));
        assert_eq!(l.next(), Some(Ok(Token{line:2, col:3, len:0, t:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:2, col:3, len:0, t:BlockEnd})));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_double_block_end() {
        let code = "task Test:\n\ttask M1:\n\t\top()\ntask Two";
        let mut l = Lexer::new(code);
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:1, len:4, t:Task})));
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:6, len:4, t:Identifier("Test")})));
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:10, len:1, t:Colon})));
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:11, len:0, t:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:2, col:1, len:1, t:BlockStart})));
        assert_eq!(l.next(), Some(Ok(Token{line:2, col:2, len:4, t:Task})));
        assert_eq!(l.next(), Some(Ok(Token{line:2, col:7, len:2, t:Identifier("M1")})));
        assert_eq!(l.next(), Some(Ok(Token{line:2, col:9, len:1, t:Colon})));
        assert_eq!(l.next(), Some(Ok(Token{line:2, col:10, len:0, t:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:3, col:2, len:1, t:BlockStart})));
        assert_eq!(l.next(), Some(Ok(Token{line:3, col:3, len:2, t:Identifier("op")})));
        assert_eq!(l.next(), Some(Ok(Token{line:3, col:5, len:1, t:OpenParenthesis})));
        assert_eq!(l.next(), Some(Ok(Token{line:3, col:6, len:1, t:CloseParenthesis})));
        assert_eq!(l.next(), Some(Ok(Token{line:3, col:7, len:0, t:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:4, col:0, len:0, t:BlockEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:4, col:0, len:0, t:BlockEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:4, col:1, len:4, t:Task})));
        assert_eq!(l.next(), Some(Ok(Token{line:4, col:6, len:3, t:Identifier("Two")})));
        assert_eq!(l.next(), Some(Ok(Token{line:4, col:9, len:0, t:StatementEnd})));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_type() {
        let code = "type Cell:\n\tc1\n\tc2";
        let mut l = Lexer::new(code);
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:1, len:4, t:Type})));
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:6, len:4, t:Identifier("Cell")})));
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:10, len:1, t:Colon})));
        assert_eq!(l.next(), Some(Ok(Token{line:1, col:11, len:0, t:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:2, col:1, len:1, t:BlockStart})));
        assert_eq!(l.next(), Some(Ok(Token{line:2, col:2, len:2, t:Identifier("c1")})));
        assert_eq!(l.next(), Some(Ok(Token{line:2, col:4, len:0, t:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:3, col:2, len:2, t:Identifier("c2")})));
        assert_eq!(l.next(), Some(Ok(Token{line:3, col:4, len:0, t:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{line:3, col:4, len:0, t:BlockEnd})));
        assert_eq!(l.next(), None);
    }
}