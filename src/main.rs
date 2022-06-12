mod htn;
use std::hash::Hash;
use std::rc::Rc;

// use htn::parser::Literal;
use htn::domain::Domain;
use htn::planner::Planner;
use htn::interpreter::{State, StateType};


fn main() {
    let domain = match Domain::from_file("htn-problems/Supplier.htn") {
        Ok(domain) => domain,
        Err(e) => {eprintln!("{}", e); panic!()},
    };
    // print!("{:?}", domain);
    let planner = Planner::new(domain);
    let vid = &planner.domain.variable_ids;
    let mut state = planner.new_state();
    use StateType::*;
    state.set(*vid.get("hunger").unwrap(), F(1.3));
    state.set(*vid.get("have_supply_need").unwrap(), B(true));
    state.set(*vid.get("carryFood").unwrap(), B(false));
    state.set(*vid.get("rHasFood").unwrap(), B(true));
    state.set(*vid.get("at").unwrap(), I(0));


    let plan = planner.plan(&state).unwrap();
    if let Some(last_task) = plan.0.iter().last() {
        if !last_task.is_complete {
            println!("Partial plan!");
        }
    }
    println!("Planer finished successfully. Plan: {:?}", plan);

}
