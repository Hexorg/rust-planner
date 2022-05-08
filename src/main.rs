
mod pddl;
use pddl::{
    domain::Domain,
    // expression::Expression,
    // action::Action,
    // problem::Problem
};
use std::fs;

fn main() {
    let pddl = fs::read_to_string("pddl-examples/rover/strips/domain.pddl")
        .expect("Error while opening .pddl file");
    let domain = pddl.parse::<Domain>().expect("Error while parsing .pddl");
    println!("{}", domain);

}
