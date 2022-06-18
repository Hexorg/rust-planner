mod htn;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

// use htn::parser::Literal;
use htn::domain::Domain;
// use htn::planner::Planner;
use htn::interpreter::{State, StateType};


fn main() {
    let mut type_counts = HashMap::<String, usize>::new();
    type_counts.insert("Cell".to_owned(), 7);
    let domain = match Domain::from_file("htn-problems/sample.htn", type_counts) {
        Ok(domain) => domain,
        Err(e) => {eprintln!("{}", e); panic!()},
    };
    print!("{:?}", domain);
    // let planner = Planner::new(domain);
    // todo!();
    // let vid = &planner.domain.variable_ids;
    // let mut state = planner.new_state();
    // use StateType::*;
    // // println!("VarMap: {:?}", vid);
    // state.set(*vid.get("carry").unwrap(), B(false));
    // state.set(*vid.get("cell1").unwrap(), B(true));
    // state.set(*vid.get("cell2").unwrap(), B(true));
    // state.set(*vid.get("cell3").unwrap(), B(false));
    // state.set(*vid.get("cell4").unwrap(), B(false));
    // state.set(*vid.get("cell5").unwrap(), B(false));
    // state.set(*vid.get("cell6").unwrap(), B(false));


    // let plan = planner.plan(&state).unwrap();
    // if let Some(last_task) = plan.0.iter().last() {
    //     if !last_task.is_complete {
    //         println!("Partial plan!");
    //     }
    // }
    // println!("Planer finished successfully. Plan: {:?}", plan);

}
