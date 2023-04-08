
#[derive(Debug, PartialEq)]
pub enum Operation {
    ReadState(usize),
    WriteState(usize),
    Not, 
    And,
    Or,
}


#[derive(Debug, PartialEq)]
pub struct Action {
    pub preconditions: Vec<Operation>,
    pub effects: Vec<Operation>,
}

#[derive(Debug, PartialEq)]
pub struct Problem {
    pub init: Vec<Operation>,
    pub goal: Vec<Operation>,
    pub actions: Vec<Action>
}


pub mod pddl;
mod types;