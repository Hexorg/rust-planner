use std::fmt;
use std::str::FromStr;
use super::action::Action;
use super::predicate::Predicate;
use super::parser::Lexer;

pub struct Domain<'a> {
    pub predicates: Vec<Predicate>,
    pub actions: Vec<Action<'a>>
}



pub struct DomainParseError(());

impl fmt::Debug for DomainParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "DomainParseError")
    }
}

impl fmt::Display for Domain<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let predicates = self.predicates.iter().take(1).fold(String::new(), |acc,item| acc + &format!("{}", item));
        let predicates = self.predicates.iter().skip(1).fold(predicates, |acc,item| acc + " " + &format!("{}", item));
        let actions = self.actions.iter().take(1).fold(String::new(), |acc,item| acc + &format!("{}", item));
        let actions = self.actions.iter().skip(1).fold(actions, |acc,item| acc + "\n" + &format!("{}", item));
        write!(f, "(:domain\n :predicates {}\n{}\n)", predicates, actions)
    }
}

impl FromStr for Domain<'_> {
    type Err = DomainParseError;
    fn from_str(pddl: &str) -> Result<Self, Self::Err> {
        Lexer::tokenize(pddl);
        let actions = Vec::<Action>::new();
        let predicates = Vec::<Predicate>::new();
        
        Ok(Domain{actions, predicates})
    }
}
