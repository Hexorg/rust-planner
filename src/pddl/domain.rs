use std::fmt;
use super::action::Action;
use super::predicate::Predicate;

pub struct Domain<'a> {
    pub predicates: Vec<Predicate>,
    pub actions: Vec<Action<'a>>
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

impl<'a> Domain<'a> {
    pub fn new() -> Domain<'a> {
        Domain{
            predicates: Vec::new(),
            actions: Vec::new()
        }
    }
    pub fn new_predicate(&mut self, name: &str, variable_count: usize) -> usize {
        let new_index = self.predicates.len();
        self.predicates.push(Predicate::new(name, variable_count));
        return new_index;
    }
}