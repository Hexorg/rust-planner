pub struct Lexer { }

pub enum Token {
    LEFT_PAREN(),
    RIGHT_PAREN(),
    LITERAL(String),
    COLON(String),
    QUESTION(String)
}

impl Lexer {
    pub fn tokenize(pddl: &str) -> Vec<Token> {
        let mut tokens = Vec::<Token>::new();
        for (idx, c) in pddl.char_indices() {
            println!("idx:{}, c:{}", idx, c);
            if let Some(next) = match c {
                '(' => Some(Token::LEFT_PAREN()),
                ')' => Some(Token::RIGHT_PAREN()),
                ':' => Some(Token::COLON()),
                a => if a.is_alphabetic() { Some(Token::WORD()) } else { None }
            } {
                tokens.push(next);
            }
        }
        return tokens;
    }
}