use std::fmt;

pub struct Lexer { }

pub enum Token {
    LEFT_PAREN(),
    RIGHT_PAREN(),
    DOMAIN(),
    LITERAL(String),
    COLON(String),
    QUESTION(String)
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::LEFT_PAREN() => write!(f, "("),
            Token::RIGHT_PAREN() => write!(f, ")"),
            Token::DOMAIN() => write!(f, "DOMAIN"),
            Token::LITERAL(lit) => write!(f, "{}", lit),
            Token::COLON(lit) => write!(f, ":{}", lit),
            Token::QUESTION(lit) => write!(f, "?{}", lit)
        }
    }
}

impl Lexer {
    pub fn tokenize(pddl: &str) -> Vec<Token> {
        let mut tokens = Vec::<Token>::new();
        for (idx, c) in pddl.char_indices() {
            println!("idx:{}, c:{}", idx, c);
            if let Some(next) = match c {
                '(' => Some(Token::LEFT_PAREN()),
                ')' => Some(Token::RIGHT_PAREN()),
                ':' => Some(Token::COLON(String::new())),
                '?' => Some(Token::QUESTION(String::new())),
                a => if a.is_alphanumeric() || a == '-' { 
                    match tokens.last_mut() {
                        Some(Token::LITERAL(word)) |
                        Some(Token::COLON(word)) |
                        Some(Token::QUESTION(word)) => word.push(a),
                        Some(_) | None => if a.is_digit(10) { } else {
                            tokens.push(Token::LITERAL(a.to_string()))
                        }
                    }
                    None
                 } else 
                 if a.is_whitespace() {
                     None
                 } else 
                 { None }
            } {
                tokens.push(next);
            }
        }
        return tokens;
    }
}