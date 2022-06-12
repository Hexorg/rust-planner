mod htn;
use std::hash::Hash;
use std::rc::Rc;

// use htn::parser::Literal;
use htn::domain::Domain;
use htn::planner::Planner;
use htn::interpreter::{State, StateType};


fn main() {
    let domain = match Domain::from_file("htn-problems/testing.htn") {
        Ok(domain) => domain,
        Err(e) => {eprintln!("{}", e); panic!()},
    };
    // print!("{:?}", domain);
    let planner = Planner::new(domain);
    let vid = &planner.domain.variable_ids;
    let mut state = planner.new_state();
    use StateType::*;
    state.set(*vid.get("cnd3").unwrap(), B(true));
    state.set(*vid.get("cnd4").unwrap(), B(true));


    let plan = planner.plan(&state).unwrap();
    if let Some(last_task) = plan.0.iter().last() {
        if !last_task.is_complete {
            println!("Partial plan!");
        }
    }
    println!("Planer finished successfully. Plan: {:?}", plan);

}
