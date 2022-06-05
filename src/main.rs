mod htn;
use std::collections::HashMap;
use std::rc::Rc;

use htn::domain::{Domain};
use htn::planner::{Planner};
use htn::planner::State;

fn main() {
    let domain = match Domain::from_file("htn-problems/Supplier.htn") {
        Ok(domain) => domain,
        Err(e) => {eprintln!("{}", e); panic!()},
    };

    //println!("{:?}", domain);
    let mut planner = Planner::new();
    let mut state = State(HashMap::new());
    state.0.insert(Rc::new(String::from("hunger")), 0);
    state.0.insert(Rc::new(String::from("have_supply_need")), 0);
    state.0.insert(Rc::new(String::from("carryFood")), 0);
    state.0.insert(Rc::new(String::from("at")), 0);
    match planner.plan(&state, &domain) {
        Ok(true) => println!("Planer finished successfully."),
        Ok(false) => println!("Planer was not able to find full solution - the plan is partial"),
        Err(e) => domain.print_parse_error(&e),
    }
    for action in planner.plan {
        match action.preconditions {
            Some(p) => print!("if {} then ", p),
            _ => ()
        }
        println!("run {}:",action.name);
        for op in action.operators {
            println!("\t{}({})", op.operator, op.arguments.iter().fold(String::new(), |acc,item| acc + &format!("{}, ", item)));
        }
        println!("\tExpecting state: {:?}", action.end_state.0);
    }
    // match plan {
    //     Ok(plan) => println!("Plan: {:?}", plan),
    //     Err(e) => domain.print_parse_error(&e),
    // }
}
