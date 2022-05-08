use std::fmt;
use super::domain::{Expression, Action, Predicate, Domain};

pub struct Lexer { }

pub struct Parser<'a> { 
    idx: usize,
    tokens: &'a Vec<Token>,
}

pub struct ParserError{
    col: usize,
    line: usize,
    message: String,
}

impl fmt::Debug for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Error on line {}:{} {}", self.line, self.col, self.message)
    }
}

enum TokenType {
    LeftParen,
    RightParen,
    Define,
    Domain,
    And,
    Or,
    Not,
    Action,
    Effect,
    Parameters,
    Precondition,
    Predicates,
    Literal(String),
    Variable(String)
}

pub struct Token {
    col: usize,
    line: usize,
    token_type: TokenType
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::Define => write!(f, "DEFINE"),
            TokenType::Domain => write!(f, "DOMAIN"),
            TokenType::Action => write!(f, ":action"),
            TokenType::Predicates => write!(f, ":predicates"),
            TokenType::Effect => write!(f, ":effect"),
            TokenType::Parameters => write!(f, ":parameters"),
            TokenType::Precondition => write!(f, ":precondition"),
            TokenType::And => write!(f, "AND"),
            TokenType::Or => write!(f, "OR"),
            TokenType::Not => write!(f, "NOT"),
            TokenType::Literal(lit) => write!(f, "literal {}", lit),
            TokenType::Variable(lit) => write!(f, "variable ?{}", lit)
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_type)
    }
}

impl Lexer {
    pub fn tokenize(pddl: &str) -> Vec<Token> {
        let mut tokens = Vec::<Token>::new();
        let mut finished_token: bool = true;
        let mut col:usize = 0;
        let mut line:usize = 1;

        for c in pddl.chars() {
            col += 1;
            match c {
                '(' => {finished_token = true; tokens.push(Token{col, line, token_type:TokenType::LeftParen})},
                ')' => {finished_token = true; tokens.push(Token{col, line, token_type:TokenType::RightParen})},
                '?' => {finished_token = false; tokens.push(Token{col, line, token_type:TokenType::Variable(String::new())})},
                c if finished_token && (c.is_alphabetic() || c == ':') => {finished_token = false; tokens.push(Token{col, line, token_type:TokenType::Literal(c.to_string())})},
                c if !finished_token && (c.is_alphanumeric() || c == '-') => {
                        if let Some(TokenType::Literal(ref mut word ) |
                            TokenType::Variable(ref mut word)) = tokens.last_mut().and_then(|x|Some(&mut x.token_type)) {
                                word.push(c);
                                let new_type = match word.as_str() {
                                    "domain" => Some(TokenType::Domain),
                                    "define" => Some(TokenType::Define),
                                    ":action" => Some(TokenType::Action),
                                    ":effect" => Some(TokenType::Effect),
                                    ":parameters" => Some(TokenType::Parameters),
                                    ":precondition" => Some(TokenType::Precondition),
                                    ":predicates" => Some(TokenType::Predicates),
                                    "and" => Some(TokenType::And),
                                    "or" => Some(TokenType::Or),
                                    "not" => Some(TokenType::Not),
                                    _ => None
                                };
                                if let Some(nt) = new_type {
                                    tokens.last_mut().unwrap().token_type = nt;
                                }
                            }
                    }
                '\n' => {finished_token = true; line += 1; col = 0},
                _ => finished_token = true,
            }
            
        }
        return tokens;
    }
}

macro_rules! expect {
    ($y:expr, $x:pat) => {
        if let $x = $y.tokens[$y.idx].token_type {
            $y.idx += 1;
            Ok(())
        } else {
            let token = &$y.tokens[$y.idx];
            Err(ParserError{col:token.col, line:token.line, message:format!("Unexpected '{}'", token.token_type)})
        }
    };
}

macro_rules! expect_get {
    ($y:expr, $x:pat, $z:ident) => {
        if let $x = &$y.tokens[$y.idx].token_type {
            $y.idx += 1;
            Ok($z)
        } else {
            let token = &$y.tokens[$y.idx];
            Err(ParserError{col:token.col, line:token.line, message:format!("Unexpected '{}'", token.token_type)})
        }
    };
}

impl Parser<'_> {

    fn expression(&mut self) -> Result<Expression, ParserError> {
        expect!(self, TokenType::LeftParen)?;
        let expr = match &self.tokens[self.idx].token_type {
            TokenType::And => { let mut operands = Vec::<Expression>::new();
                self.idx += 1;
                while let Ok(subexp) = self.expression() {
                    operands.push(subexp);
                }
                Expression::And(operands)
            },
            TokenType::Or => { let mut operands = Vec::<Expression>::new();
                self.idx += 1;
                while let Ok(subexp) = self.expression() {
                    operands.push(subexp);
                }
                Expression::Or(operands)
            },
            TokenType::Not => {self.idx += 1; Expression::Not(Box::new(self.expression()?))},
            TokenType::Literal(name) => { 
                self.idx += 1;
                let mut vars = Vec::<String>::new();
                while let TokenType::Variable(var) = &self.tokens[self.idx].token_type {
                    self.idx += 1;
                    vars.push(var.clone());
                }
                Expression::Predicate(name.clone(), vars)
                
            },
            TokenType::RightParen => {self.idx += 1; Expression::None},
            _ => return Err(ParserError{col:self.tokens[self.idx].col, line:self.tokens[self.idx].col, message:"Expected and, or, not, or ')'".to_string()})
        };
        expect!(self, TokenType::RightParen)?;
        Ok(expr)
    }

    fn actions(&mut self) -> Result<Vec<Action>, ParserError> {
        let mut r = Vec::<Action>::new();
        while let TokenType::LeftParen = self.tokens[self.idx].token_type {
            expect!(self, TokenType::LeftParen)?;
            expect!(self, TokenType::Action)?;
            let name = expect_get!(self, TokenType::Literal(name), name)?.clone();
            expect!(self, TokenType::Parameters)?;
            expect!(self, TokenType::LeftParen)?;
            let mut parameters = Vec::<String>::new();
            while let TokenType::Variable(name) = &self.tokens[self.idx].token_type {
                self.idx += 1;
                parameters.push(name.clone());
            }
            expect!(self, TokenType::RightParen)?;
            expect!(self, TokenType::Precondition)?;
            let precondition = self.expression()?;
            expect!(self, TokenType::Effect)?;
            let effect = self.expression()?;
            expect!(self, TokenType::RightParen)?;
            r.push(Action{name, parameters, effect, precondition});
        }
        Ok(r)
    }

    fn predicates(&mut self) -> Result<Vec<Predicate>, ParserError> {
        expect!(self, TokenType::LeftParen)?;
        expect!(self, TokenType::Predicates)?;
        let mut predicates = Vec::<Predicate>::new();
        while let TokenType::LeftParen = self.tokens[self.idx].token_type {
            self.idx += 1;
            let name = expect_get!(self, TokenType::Literal(name), name)?.clone();
            let mut arity = 0;
            while let TokenType::Variable(_) = &self.tokens[self.idx].token_type {
                self.idx += 1;
                arity += 1;
            }
            predicates.push(Predicate{name, arity});
            expect!(self, TokenType::RightParen)?;
        }
        expect!(self, TokenType::RightParen)?;
        Ok(predicates)
    }

    fn domain(&mut self) -> Result<Domain, ParserError> {
        expect!(self, TokenType::LeftParen)?;
        expect!(self, TokenType::Domain)?;
        let name = expect_get!(self, TokenType::Literal(name), name)?.clone();
        expect!(self, TokenType::RightParen)?;
        let predicates = self.predicates()?;
        let actions = self.actions()?;
        Ok(Domain{name, predicates, actions})
    }


    fn grammar(&mut self) -> Result<Domain, ParserError> {
        expect!(self, TokenType::LeftParen)?;
        expect!(self, TokenType::Define)?;
        let r = if let TokenType::Domain = self.tokens[self.idx+1].token_type {
            self.domain()?
        } else {
            Domain{name:"none".to_string(), actions:Vec::new(), predicates:Vec::new()}
        };
        expect!(self, TokenType::RightParen)?;
        Ok(r)
    }

    pub fn parse(tokens: &Vec<Token>) -> Result<Domain, ParserError> {
        let mut parser = Parser{idx:0, tokens};
        parser.grammar()
    }
}