use std::iter::Peekable;
use std::str::CharIndices;

use super::tokens::{Token, Span, TokenKind, Literal};
use super::Error;
use TokenKind::*;
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
                    ':' => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:Colon})),
                    '(' => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:OpenParenthesis})),
                    ')' => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:CloseParenthesis})),
                    ',' => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:Comma})),
                    '|' => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:Or})),
                    '&' => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:And})),
                    '.' => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:Dot})),
                    '=' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{span:Span::new(self.line, self.col, 2), kind:EqualsEquals}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:Equals})),
                    },
                    '-' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{span:Span::new(self.line, self.col, 2), kind:SubtractFrom}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:Minus})),
                    },
                    '+' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{span:Span::new(self.line, self.col, 2), kind:AddTo}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:Plus})),
                    },
                    '/' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{span:Span::new(self.line, self.col, 2), kind:DivideBy}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:Slash})),
                    },
                    '*' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{span:Span::new(self.line, self.col, 2), kind:MultiplyBy}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:Star})),
                    },
                    '>' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{span:Span::new(self.line, self.col, 2), kind:GreaterOrEquals}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:Greater})),
                    },
                    '<' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{span:Span::new(self.line, self.col, 2), kind:SmallerOrEquals}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:Smaller})),
                    },
                    '!' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{span:Span::new(self.line, self.col, 2), kind:NotEquals}; self.it.next(); Some(Ok(t))},
                        _ => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:ExclamationPoint}))
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
                    _ => {let e = Some(Err(Error::new(Span::new(self.line, self.col, 1), "Unexpected character."))); e}
                };
                if let Some(result) = &new_token {
                    match result {
                        Ok(t) => self.col += t.span.len,
                        Err(_) => self.col += 1,
                    }
                }
                if let Some((_, '\n')) | None  = self.it.peek() {
                    if let Some(Token{kind:StatementEnd,..}) = self.stash.last() { } else {
                        self.stash.push(Token{span:Span::new(self.line, self.col, 0), kind:StatementEnd});
                    }
                }
                if let Some(Ok(token)) = new_token {
                    if self.is_newline && self.tab_depth > 0 {
                        match token { 
                            Token{kind:BlockStart,..} => (),
                            _ => {
                                self.stash.push(token); 
                                // self.col -= token.len();
                                self.tab_depth-=1; 
                                new_token = Some(Ok(Token{span:Span::new(self.line, 0, 0), kind:BlockEnd}));
                                while self.tab_depth > 0 {
                                    self.stash.push(Token{span:Span::new(self.line, 0, 0), kind:BlockEnd});
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
                    Some(Ok(Token{span:Span::new(self.line, self.col, 0), kind:BlockEnd}))
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
        let r = Ok(Token{span:Span::new(self.line, self.col, len), kind:Literal(Literal::S(slice)), });
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
                Ok(literal) => Ok(Token{span:Span::new(self.line, self.col, len), kind:Literal(Literal::F(literal))}),
                Err(_) => Err(Error::new(Span::new(self.line, self.col, 1), "Unable to parse float."))
            }
        } else {
            match slice.parse::<i32>() {
                Ok(literal) => Ok(Token{span:Span::new(self.line, self.col, len), kind:Literal(Literal::I(literal))}),
                Err(_) => Err(Error::new(Span::new(self.line, self.col, 1), "Unable to parse integer."))
            }
        }
    }

    fn identifier(&mut self, offset:usize) -> Result<Token<'a>, Error> {
        let mut len = 1;
        while let Some(_) = self.it.next_if(|(_,c)| (c.is_alphanumeric() || *c == '_')) { len += 1; }
        let slice = if let Some((identifier_end,_)) = self.it.peek() { &self.text[offset..*identifier_end]} else { &self.text[offset..]};
        let token = match slice {
            "task" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Task}),
            "when" => Ok(Token{span:Span::new(self.line, self.col, len), kind:When}),
            "else" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Else}),
            "do" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Do}),
            "def" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Def}),
            "ordered" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Ordered}),
            "on" => Ok(Token{span:Span::new(self.line, self.col, len), kind:On}),
            "include" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Include}),
            "pass" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Pass}),
            "cost" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Cost}),
            "or" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Or}),
            "and" => Ok(Token{span:Span::new(self.line, self.col, len), kind:And}),
            "not" => Ok(Token{span:Span::new(self.line, self.col, len), kind:ExclamationPoint}),
            "false" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Literal(Literal::B(false))}),
            "true" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Literal(Literal::B(true))}),
            _ => Ok(Token{span:Span::new(self.line, self.col, len), kind:Identifier(slice)})
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
                (false, false) => Some(Error::new(Span::new(self.line, self.col, 1), "Only tabs or spaces are allowed at the beginning of the line.")),
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
                self.stash.push(Token{span:Span::new(self.line, self.col, 1), kind:BlockStart});
                self.col += 1;
                self.tab_depth += 1;
            }
            while self.tab_depth > count {
                self.stash.push(Token{span:Span::new(self.line, 0, 0), kind:BlockEnd});
                self.tab_depth -= 1;
            }
            return None
        }
        return Some(Error::new(Span::new(self.line, self.col, 1), "Expected spaces at the beginning of the line."))
    }

    fn spaces(&mut self, count:usize) -> Option<Error> {
        if self.depth_separator.is_none() {
            self.depth_separator = Some(DepthSeparator::SPACES(count));
        }
        if let Some(DepthSeparator::SPACES(one_depth)) = self.depth_separator {
            if count % one_depth != 0 {
                return Some(Error::new(Span::new(self.line, self.col, count), &format!("Extra spaces. Expected {}.", one_depth)))
            }
            let count = count / one_depth;
            if self.tab_depth <= count {
                self.col += self.tab_depth*one_depth;
            } else if self.tab_depth > count {
                self.col += count*one_depth;
            }
            while self.tab_depth < count {
                self.stash.push(Token{span:Span::new(self.line, self.col, one_depth), kind:BlockStart});
                self.col += one_depth;
                self.tab_depth += 1;
            }
            while self.tab_depth > count {
                self.stash.push(Token{span:Span::new(self.line, 0, 0), kind:BlockEnd});
                self.tab_depth -= 1;
            }
            return None
        }
        return Some(Error::new(Span::new(self.line, self.col, 1), "Expected tabs at the beginning of the line."))
    }
}

#[cfg(test)]
mod tests {
    use crate::htn::parser::{lexer::DepthSeparator, Error};

    use super::{Lexer, Token, Span, TokenKind::*, Literal::*};
    #[test]
    fn test_include() {
        let code = "include \"str\"";
        let mut l = Lexer::new(code);
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 1, 7), kind:Include})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 10, 3), kind:Literal(S("str"))})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 14, 0), kind:StatementEnd})));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_comments() {
        let code = "# haha\n  # haha\n\ntask";
        let mut l = Lexer::new(code);
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(4, 1, 4), kind:Task})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(4, 5, 0), kind:StatementEnd})));
        assert_eq!(l.next(), None);
        assert_eq!(l.depth_separator, None);
    }

    #[test]
    fn test_space_block() {
        let code = "  &\n    &\n  &\n&";
        let mut l = Lexer::new(code);
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 1, 2), kind:BlockStart})));
        assert_eq!(l.depth_separator, Some(DepthSeparator::SPACES(2)));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 3, 1), kind:And})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 4, 0), kind:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 3, 2), kind:BlockStart})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 5, 1), kind:And})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 6, 0), kind:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 0, 0), kind:BlockEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 3, 1), kind:And})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 4, 0), kind:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(4, 0, 0), kind:BlockEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(4, 1, 1), kind:And})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(4, 2, 0), kind:StatementEnd})));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_tab_block() {
        let code = "\t&\n\t\t&\n\t&\n&";
        let mut l = Lexer::new(code);
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 1, 1), kind:BlockStart})));
        assert_eq!(l.depth_separator, Some(DepthSeparator::TABS));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 2, 1), kind:And})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 3, 0), kind:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 2, 1), kind:BlockStart})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 3, 1), kind:And})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 4, 0), kind:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 0, 0), kind:BlockEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 2, 1), kind:And})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 3, 0), kind:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(4, 0, 0), kind:BlockEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(4, 1, 1), kind:And})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(4, 2, 0), kind:StatementEnd})));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_error_block() {
        let code = " &\n\t&";
        let mut l = Lexer::new(code);
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 1, 1), kind:BlockStart})));
        assert_eq!(l.depth_separator, Some(DepthSeparator::SPACES(1)));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 2, 1), kind:And})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 3, 0), kind:StatementEnd})));
        assert_eq!(l.next(), Some(Err(Error::new(Span::new(2, 1, 1), "Expected spaces at the beginning of the line."))));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 2, 1), kind:And})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 3, 0), kind:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 3, 0), kind:BlockEnd})));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_double_block_end() {
        let code = "task Test:\n\ttask M1:\n\t\top()\ntask Two";
        let mut l = Lexer::new(code);
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 1, 4), kind:Task})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 6, 4), kind:Identifier("Test")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 10, 1), kind:Colon})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 11, 0), kind:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 1, 1), kind:BlockStart})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 2, 4), kind:Task})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 7, 2), kind:Identifier("M1")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 9, 1), kind:Colon})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 10, 0), kind:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 2, 1), kind:BlockStart})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 3, 2), kind:Identifier("op")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 5, 1), kind:OpenParenthesis})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 6, 1), kind:CloseParenthesis})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 7, 0), kind:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(4, 0, 0), kind:BlockEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(4, 0, 0), kind:BlockEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(4, 1, 4), kind:Task})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(4, 6, 3), kind:Identifier("Two")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(4, 9, 0), kind:StatementEnd})));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_ops() {
        let code = "><-*/p=+!=<=>=!true";
        let mut l = Lexer::new(code);
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 1, 1), kind:Greater})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 2, 1), kind:Smaller})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 3, 1), kind:Minus})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 4, 1), kind:Star})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 5, 1), kind:Slash})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 6, 1), kind:Identifier("p")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 7, 1), kind:Equals})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 8, 1), kind:Plus})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 9, 2), kind:NotEquals})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 11, 2), kind:SmallerOrEquals})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 13, 2), kind:GreaterOrEquals})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 15, 1), kind:ExclamationPoint})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 16, 4), kind:Literal(B(true))})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 20, 0), kind:StatementEnd})));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn sample_task() {
        let code = "task t(a):\n\twhen:\n\t\ta.p(x)\n\tdo:\n\t\tordered:\n\t\t\tg.f(x)";
        let mut l = Lexer::new(code);
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 1, 4), kind:Task})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 6, 1), kind:Identifier("t")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 7, 1), kind:OpenParenthesis})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 8, 1), kind:Identifier("a")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 9, 1), kind:CloseParenthesis})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 10, 1), kind:Colon})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 11, 0), kind:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 1, 1), kind:BlockStart})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 2, 4), kind:When})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 6, 1), kind:Colon})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 7, 0), kind:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 2, 1), kind:BlockStart})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 3, 1), kind:Identifier("a")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 4, 1), kind:Dot})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 5, 1), kind:Identifier("p")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 6, 1), kind:OpenParenthesis})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 7, 1), kind:Identifier("x")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 8, 1), kind:CloseParenthesis})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(3, 9, 0), kind:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(4, 0, 0), kind:BlockEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(4, 2, 2), kind:Do})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(4, 4, 1), kind:Colon})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(4, 5, 0), kind:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(5, 2, 1), kind:BlockStart})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(5, 3, 7), kind:Ordered})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(5, 10, 1), kind:Colon})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(5, 11, 0), kind:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(6, 3, 1), kind:BlockStart})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(6, 4, 1), kind:Identifier("g")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(6, 5, 1), kind:Dot})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(6, 6, 1), kind:Identifier("f")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(6, 7, 1), kind:OpenParenthesis})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(6, 8, 1), kind:Identifier("x")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(6, 9, 1), kind:CloseParenthesis})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(6, 10, 0), kind:StatementEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(6, 10, 0), kind:BlockEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(6, 10, 0), kind:BlockEnd})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(6, 10, 0), kind:BlockEnd})));
        assert_eq!(l.next(), None);
    }
}