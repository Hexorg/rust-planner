mod htn;
use std::collections::HashMap;

// use htn::parser::Literal;
use htn::domain::{Domain, OperandType};
use htn::planner::Planner;
// use htn::interpreter::{State, StateType};


fn main() {
    let type_map = HashMap::<&str, Vec<&str>>::new();
    //type_map.insert("Cell", vec!["cell0", "cell1", "cell2", "cell3", "cell4", "cell5", "cell6"]);
    let domain = match Domain::from_file("htn-problems/SAT.htn", type_map) {
        Ok(domain) => domain,
        Err(e) => {eprintln!("{}", e); panic!()},
    };
    
    print!("{:?}", domain);
    // return;
    let planner = Planner::new(domain);
    // todo!();
    let vid = planner.domain.get_state_mapping();
    let operators = planner.domain.get_operator_mapping();
    let blackboard = planner.domain.get_blackboard_mapping();
    let mut state = planner.new_state();
    use OperandType::*;
    // state.set(*vid.get("carry").unwrap(), I(0));
    // state.set(*vid.get("left.bottom").unwrap(), I(3));
    // state.set(*vid.get("left.middle").unwrap(), I(2));
    // state.set(*vid.get("left.top").unwrap(), I(1));
    state.set(*vid.get("a1").unwrap(), B(false));
    state.set(*vid.get("a2").unwrap(), B(false));
    state.set(*vid.get("a3").unwrap(), B(false));
    // state.set(*vid.get("cell1").unwrap(), B(true));
    // state.set(*vid.get("cell2").unwrap(), B(false));
    // state.set(*vid.get("cell3").unwrap(), B(false));
    // state.set(*vid.get("cell4").unwrap(), B(false));
    // state.set(*vid.get("cell5").unwrap(), B(false));
    // state.set(*vid.get("cell6").unwrap(), B(false));


    let plan = planner.plan(&state).unwrap();
    println!("Planer finished successfully. Plan: {:?}\nDecompiled plan:", plan);
    let mut stack = Vec::new();
    for op in plan.0 {
        match op {
            htn::domain::Operation::ReadBlackboard(idx) => stack.push(blackboard[idx]),
            htn::domain::Operation::WriteBlackboard(idx) => println!("^ -> Store into {}", blackboard[idx]),
            htn::domain::Operation::CallOperator(idx, arity) => println!("{}({})", operators[idx], {
                let mut i = stack.iter().take(arity);
                let args = i.by_ref().take(1).fold(String::new(), |acc,item| acc + item);
                let args = i.fold(args, |acc,item| acc + ", " + item);
                for _ in 0..arity { stack.pop(); }
                args
            }),
            _ => println!("Uxexpected operation: {:?}", op)
        }
    }
    

}


// first slide: 