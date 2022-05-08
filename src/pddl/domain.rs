use std::fmt;
use std::str::FromStr;
use super::parser::{Lexer, Parser, ParserError};


pub enum Expression {
    And(Vec<Expression>),
    Or(Vec<Expression>),
    Not(Box<Expression>),
    Predicate(String, Vec<String>),
    None,
}



impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::And(exp) => {
                write!(f, "(and ")?;
                for (idx, e) in exp.iter().enumerate() {
                    write!(f, "{}", e)?;
                    if idx + 1 < exp.len() {
                        write!(f, ",")?;
                    }
                }
                write!(f, ")")
            },
            Self::Or(exp) => {
                write!(f, "(or ")?;
                for (idx, e) in exp.iter().enumerate() {
                    write!(f, "{}", e)?;
                    if idx + 1 < exp.len() {
                        write!(f, ",")?;
                    }
                }
                write!(f, ")")
            },
            Self::Not(exp) => write!(f, "(not {})", exp),
            Self::Predicate(id, args) => {
                write!(f, "(pred ")?;
                for (idx, a) in args.iter().enumerate() {
                    write!(f, "{}", a)?;
                    if idx + 1 < args.len() {
                        write!(f, ",")?;
                    }
                }
                write!(f, ")")
            },
            Self::None => write!(f, "()")
        }
    }
}
pub struct Action {
    pub name: String,
    pub parameters: Vec<String>,
    pub precondition: Expression,
    pub effect: Expression,
}

impl fmt::Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(:action {} :parameters {:?} :precondition {} :effect {}", self.name, self.parameters, self.precondition, self.effect)
    }
}
pub struct Predicate {
    pub name: String,
    pub arity: usize,
}

impl fmt::Display for Predicate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.arity={}", self.name, self.arity)
    }
}

pub struct Domain {
    pub name: String,
    pub predicates: Vec<Predicate>,
    pub actions: Vec<Action>
}


impl fmt::Display for Domain {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let predicates = self.predicates.iter().take(1).fold(String::new(), |acc,item| acc + &format!("{}", item));
        let predicates = self.predicates.iter().skip(1).fold(predicates, |acc,item| acc + " " + &format!("{}", item));
        let actions = self.actions.iter().take(1).fold(String::new(), |acc,item| acc + &format!("{}", item));
        let actions = self.actions.iter().skip(1).fold(actions, |acc,item| acc + "\n" + &format!("{}", item));
        write!(f, "(define (domain {})\n (:predicates {})\n{}\n)", self.name, predicates, actions)
    }
}

impl FromStr for Domain {
    type Err = ParserError;
    fn from_str(pddl: &str) -> Result<Self, Self::Err> {
        let tokens = Lexer::tokenize(pddl);
        Parser::parse(&tokens)
    }
}
