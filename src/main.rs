mod htn;
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
    print!("{:?}", domain);
    let planner = Planner::new(domain);
    let vid = &planner.domain.variable_ids;
    println!("Variable map: {:?}", vid);
    let state_size = planner.domain.variable_ids.len();
    let mut state = State::new(state_size);
    state.set(*vid.get("hunger").unwrap(), 6 as i32);
    state.0.insert(*vid.get("have_supply_need").unwrap(), 0 as i32);
    state.0.insert(*vid.get("carryFood").unwrap(), 0 as i32);
    state.0.insert(*vid.get("rHasFood").unwrap(), 1 as i32);
    state.0.insert(*vid.get("at").unwrap(), 0 as i32);


    let plan = planner.plan(&state).unwrap();
    if let Some(last_task) = plan.0.iter().last() {
        if !last_task.is_complete {
            println!("Partial plan!");
        }
    }
    println!("Planer finished successfully. Plan: {:?}", plan);


    // for action in planner.plan {
    //     match action.preconditions {
    //         Some(p) => print!("if {} then ", p),
    //         _ => ()
    //     }
    //     println!("run {}:",action.name);
    //     for op in action.operators {
    //         println!("\t{}({})", op.operator, op.arguments.iter().fold(String::new(), |acc,item| acc + &format!("{}, ", item)));
    //     }
    //     println!("\tExpecting state: {:?}", action.end_state.0);
    // }
    // match plan {
    //     Ok(plan) => println!("Plan: {:?}", plan),
    //     Err(e) => domain.print_parse_error(&e),
    // }
}
