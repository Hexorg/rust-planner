use std::iter::Peekable;
use std::str::CharIndices;

use super::super::tokens::{Token, Span, TokenKind, BinOpToken, KeywordToken, Literal};
use super::super::{Error, Position};
use TokenKind::*;
use BinOpToken::*;
use KeywordToken::*;


pub struct Lexer<'a> { 
    text:&'a str,
    it: Peekable<CharIndices<'a>>,
    line: usize, // current source line, used for error reporting by Tokens
    col: usize, // current source column, used for error reporting by Tokens
    stash: Vec<Token<'a>>, // stash of tokens to return in leu of it.peek_two()
}

impl<'a> Lexer<'a> {
    pub fn new(text:&'a str) -> Self {
        Self{
            text,
            it:text.char_indices().peekable(), 
            line:1,
            col:1,
            stash:Vec::new(),
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
                let new_token = match c {
                    ':' => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:Colon})),
                    '(' => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:OpenParenthesis})),
                    ')' => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:CloseParenthesis})),
                    ',' => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:Comma})),
                    '|' => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:BinOp(Or)})),
                    '&' => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:BinOp(And)})),
                    '.' => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:Dot})),
                    '?' => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:QuestionMark})),
                    '=' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{span:Span::new(self.line, self.col, 2), kind:BinOp(EqualsEquals)}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:Equals})),
                    },
                    '-' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{span:Span::new(self.line, self.col, 2), kind:BinOp(SubtractFrom)}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:BinOp(Minus)})),
                    },
                    '+' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{span:Span::new(self.line, self.col, 2), kind:BinOp(AddTo)}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:BinOp(Plus)})),
                    },
                    '/' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{span:Span::new(self.line, self.col, 2), kind:BinOp(DivideBy)}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:BinOp(Slash)})),
                    },
                    '*' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{span:Span::new(self.line, self.col, 2), kind:BinOp(MultiplyBy)}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:BinOp(Star)})),
                    },
                    '>' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{span:Span::new(self.line, self.col, 2), kind:BinOp(GreaterOrEquals)}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:BinOp(Greater)})),
                    },
                    '<' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{span:Span::new(self.line, self.col, 2), kind:BinOp(SmallerOrEquals)}; self.it.next(); Some(Ok(t))}
                        _ => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:BinOp(Smaller)})),
                    },
                    '!' => match self.it.peek() {
                        Some((_, '=')) => {let t = Token{span:Span::new(self.line, self.col, 2), kind:BinOp(NotEquals)}; self.it.next(); Some(Ok(t))},
                        _ => Some(Ok(Token{span:Span::new(self.line, self.col, 1), kind:BinOp(Not)}))
                    },
                    '"' => {Some(self.string(offset))},
                    c if c.is_whitespace() => { 
                        self.col += 1;
                        return self.next()
                    }
                    c if (c.is_alphabetic() || c == '_') => Some(self.identifier(offset)),
                    c if c.is_digit(10) => Some(self.number(offset)),
                    _ => {let e = Some(Err(Error::new(Position::Span(Span::new(self.line, self.col, 1)), "Unexpected character."))); e}
                };
                if let Some(result) = &new_token {
                    match result {
                        Ok(t) => self.col += t.span.len,
                        Err(_) => self.col += 1,
                    }
                }
                new_token
            } else {
                None
            }
        }
    }
}

impl<'a> Lexer<'a> {
    fn next_char(&mut self) -> Option<(usize, char)> {
        let c = self.it.next();
        match c {
            // Newline handler:
            Some((_, '\n')) => {self.col = 1; self.line += 1; self.next_char()},

            // Comments:
            Some((_, ';')) => {while let Some(_) = self.it.next_if(|(_,c)| *c != '\n') {}; self.next_char()},
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
            match slice.parse::<f64>() {
                Ok(literal) => Ok(Token{span:Span::new(self.line, self.col, len), kind:Literal(Literal::F(literal))}),
                Err(_) => Err(Error::new(Position::Span(Span::new(self.line, self.col, 1)), "Unable to parse float."))
            }
        } else {
            match slice.parse::<i64>() {
                Ok(literal) => Ok(Token{span:Span::new(self.line, self.col, len), kind:Literal(Literal::I(literal))}),
                Err(_) => Err(Error::new(Position::Span(Span::new(self.line, self.col, 1)), "Unable to parse integer."))
            }
        }
    }

    fn identifier(&mut self, offset:usize) -> Result<Token<'a>, Error> {
        let mut len = 1;
        while let Some(_) = self.it.next_if(|(_,c)| (c.is_alphanumeric() || *c == '_' || *c == '-')) { len += 1; }
        let slice = if let Some((identifier_end,_)) = self.it.peek() { &self.text[offset..*identifier_end]} else { &self.text[offset..]};
        let token = match slice {
            "define" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Define)}),
            "domain" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Domain)}),
            "problem" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Problem)}),
            "not" => Ok(Token{span:Span::new(self.line, self.col, len), kind:BinOp(Not)}),
            "requirements" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Requirements)}),
            "types" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Types)}),
            "objects" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Objects)}),
            "init" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Init)}),
            "goal" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Goal)}),
            "and" => Ok(Token{span:Span::new(self.line, self.col, len), kind:BinOp(And)}),
            "predicates" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Predicates)}),
            "action" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Action)}),
            "strips" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Strips)}),
            "typing" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Typing)}),
            "negative-preconditions" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(NegativePreconditions)}),
            "disjunctive-preconditions" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(DisjunctivePreconditions)}),
            "action-costs" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(ActionCosts)}),
            "equality" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Equality)}),
            "existential-preconditions" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(ExistentialPreconditions)}),
            "universal-preconditions" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(UniversalPreconditions)}),
            "quantified-preconditions" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(QuantifiedPreconditions)}),
            "conditional-effects" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(ConditionalEffects)}),
            "fluents" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Fluents)}),
            "adl" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(ADL)}),
            "durative-actions" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(DurativeActions)}),
            "derived-predicates" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(DerivedPredicates)}),
            "timed-initial-literals" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(TimedInitialLiterals)}),
            "preferences" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Preferences)}),
            "constraints" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Constraints)}),
            "parameters" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Parameters)}),
            "precondition" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Precondition)}),
            "effect" => Ok(Token{span:Span::new(self.line, self.col, len), kind:Keyword(Effect)}),
            _ => Ok(Token{span:Span::new(self.line, self.col, len), kind:Identifier(slice)})
        };
        token
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Token, Span, TokenKind::*, BinOpToken::*};
    #[test]
    fn test_include() {
        let code = "((:operator (!drive ?x ?y ?z ?start ?time)\n))";
        let mut l = Lexer::new(code);
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 1, 1), kind:OpenParenthesis})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 2, 1), kind:OpenParenthesis})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 3, 1), kind:Colon})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 4, 8), kind:Identifier("operator")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 13, 1), kind:OpenParenthesis})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 14, 1), kind:BinOp(Not)})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 15, 5), kind:Identifier("drive")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 21, 1), kind:QuestionMark})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 22, 1), kind:Identifier("x")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 24, 1), kind:QuestionMark})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 25, 1), kind:Identifier("y")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 27, 1), kind:QuestionMark})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 28, 1), kind:Identifier("z")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 30, 1), kind:QuestionMark})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 31, 5), kind:Identifier("start")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 37, 1), kind:QuestionMark})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 38, 4), kind:Identifier("time")})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(1, 42, 1), kind:CloseParenthesis})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 1, 1), kind:CloseParenthesis})));
        assert_eq!(l.next(), Some(Ok(Token{span:Span::new(2, 2, 1), kind:CloseParenthesis})));
        assert_eq!(l.next(), None);
    }

}