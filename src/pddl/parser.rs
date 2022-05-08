use std::fmt;

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
            TokenType::Literal(lit) => write!(f, "{}", lit),
            TokenType::Variable(lit) => write!(f, "?{}", lit)
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

    fn predicates(&mut self) -> Result<i32, ParserError> {
        expect!(self, TokenType::LeftParen)?;
        expect!(self, TokenType::Predicates)?;

        expect!(self, TokenType::RightParen)?;
        Ok(4)
    }

    fn actions(&mut self) -> Result<i32, ParserError> {
        expect!(self, TokenType::LeftParen)?;
        expect!(self, TokenType::Action)?;

        expect!(self, TokenType::RightParen)?;
        Ok(4)
    }

    fn domain(&mut self) -> Result<i32, ParserError> {
        expect!(self, TokenType::LeftParen)?;
        expect!(self, TokenType::Define)?;
        expect!(self, TokenType::LeftParen)?;
        expect!(self, TokenType::Domain)?;
        let domain_name = expect_get!(self, TokenType::Literal(word), word)?;
        expect!(self, TokenType::RightParen)?;
        self.predicates();
        self.actions();
        expect!(self, TokenType::RightParen)?;
        Ok(4)
    }

    pub fn parse(tokens: &Vec<Token>) -> Result<i32, ParserError> {
        let mut parser = Parser{idx:0, tokens};
        parser.domain()

    }
}