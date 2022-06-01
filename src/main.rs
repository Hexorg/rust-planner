mod htn;
use std::collections::HashMap;
use std::rc::Rc;

use htn::domain::{Domain};
use htn::planner::{Planner, State};

fn main() {
    println!("Starting...");
    let domain = match Domain::from_file("htn-problems/Supplier.htn") {
        Ok(domain) => domain,
        Err(e) => {eprintln!("{}", e); panic!()},
    };
    // println!("-->{:>depth$}{}<--", "hello", depth=5);
    println!("{:?}", domain);
    let mut state = State(HashMap::new());
    state.0.insert(Rc::new(String::from("hunger")), 10);
    let plan = match Planner::run(&domain, state) {
        Ok(plan) => plan,
        Err(e) => {domain.print_parse_error(&e); panic!()},
    };
    for action in plan {
        println!("{}", action.operator);
    }
    // match plan {
    //     Ok(plan) => println!("Plan: {:?}", plan),
    //     Err(e) => domain.print_parse_error(&e),
    // }
}
