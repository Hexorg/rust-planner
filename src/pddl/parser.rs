use std::fmt;

pub struct Lexer { }

pub enum Token {
    LeftParen(usize, usize),
    RightParen(usize, usize),
    Define(usize, usize),
    Domain(usize, usize),
    And(usize, usize),
    Or(usize, usize),
    Not(usize, usize),
    Action(usize, usize),
    Effect(usize, usize),
    Parameters(usize, usize),
    Precondition(usize, usize),
    Predicates(usize, usize),
    Literal(usize, usize, String),
    Variable(usize, usize, String)
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::LeftParen(_, _) => write!(f, "("),
            Token::RightParen(_, _) => write!(f, ")"),
            Token::Define(_, _) => write!(f, "DEFINE"),
            Token::Domain(_, _) => write!(f, "DOMAIN"),
            Token::Action(_, _) => write!(f, ":action"),
            Token::Predicates(_, _) => write!(f, ":predicates"),
            Token::Effect(_, _) => write!(f, ":effect"),
            Token::Parameters(_, _) => write!(f, ":parameters"),
            Token::Precondition(_, _) => write!(f, ":precondition"),
            Token::And(_, _) => write!(f, "AND"),
            Token::Or(_, _) => write!(f, "OR"),
            Token::Not(_, _) => write!(f, "NOT"),
            Token::Literal(_, _, lit) => write!(f, "{}", lit),
            Token::Variable(_, _, lit) => write!(f, "?{}", lit)
        }
    }
}

impl Lexer {
    pub fn tokenize(pddl: &str) -> Vec<Token> {
        let mut tokens = Vec::<Token>::new();
        let mut finished_token: bool = true;
        let mut col:usize = 0;
        let mut line:usize = 0;

        // let add_wide_token = || {
        //     if let Some(token) = wide_token {
        //         if let Token::LITERAL(word) = token {
        //             match word.to_lowercase().as_str() {
        //                 "domain" => tokens.push(Token::DOMAIN()),
        //                 _ => (),
        //             }
        //         } else {
        //             tokens.push(token);
        //         }
        //         wide_token = None;
        //     }
        // };
        for c in pddl.chars() {
            col += 1;
            match c {
                '(' => {finished_token = true; tokens.push(Token::LeftParen(col, line))},
                ')' => {finished_token = true; tokens.push(Token::RightParen(col, line))},
                '?' => {finished_token = false; tokens.push(Token::Variable(col, line, String::new()))},
                c if finished_token && (c.is_alphabetic() || c == ':') => {finished_token = false; tokens.push(Token::Literal(col, line, c.to_string()))},
                c if !finished_token && (c.is_alphanumeric() || c == '-') => {
                        if let Some(Token::Literal(_, _, ref mut word ) |
                            Token::Variable(_, _, ref mut word)) = tokens.last_mut() {
                                word.push(c);
                            }
                        if let Some(Token::Literal(nc, nl, word)) = tokens.last() {
                            let old_col = *nc;
                            let old_line = *nl;
                            match word.to_lowercase().as_str() {
                                "domain" => { tokens.pop(); tokens.push(Token::Domain(old_col, old_line)); },
                                "define" => { tokens.pop(); tokens.push(Token::Define(old_col, old_line)); },
                                ":action" => { tokens.pop(); tokens.push(Token::Action(old_col, old_line)); },
                                ":effect" => { tokens.pop(); tokens.push(Token::Effect(old_col, old_line)); },
                                ":parameters" => { tokens.pop(); tokens.push(Token::Parameters(old_col, old_line)); },
                                ":precondition" => { tokens.pop(); tokens.push(Token::Precondition(old_col, old_line)); },
                                ":predicates" => { tokens.pop(); tokens.push(Token::Predicates(old_col, old_line)); },
                                "and" => { tokens.pop(); tokens.push(Token::And(old_col, old_line)); },
                                "or" => { tokens.pop(); tokens.push(Token::Or(old_col, old_line)); },
                                "not" => { tokens.pop(); tokens.push(Token::Not(old_col, old_line)); }
                                _ => ()
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