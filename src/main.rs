mod htn;
use std::collections::HashMap;

use htn::parser::Literal;
use htn::domain::Domain;
use htn::planner::Planner;



// fn main() {
//     match htn::domain::Domain::from_file("htn-problems/testing.htn", HashMap::new()) {
//         Ok(domain) => println!("{:?}", domain),
//         Err(e) => println!("{:?}", e),
//     }
// }

fn main() {
    let type_map = HashMap::<&str, Vec<&str>>::new();
    //type_map.insert("Cell", vec!["cell0", "cell1", "cell2", "cell3", "cell4", "cell5", "cell6"]);
    let args:Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} path/to/htn [initial_state_key initial_state_value [k v]]", args[0]);
    } else {
        let domain = match Domain::from_file(&args[1], type_map) {
            Ok(domain) => domain,
            Err(e) => {eprintln!("{}", e); panic!()},
        };

        print!("{:?}", domain);
        let planner = Planner::new(domain);
        let mut state = planner.new_state();
        let mut pos = 2;
        while let Some(key) = args.get(pos) {
            if let Some(value) = args.get(pos + 1) {
                if value.contains('.') {
                    if let Ok(literal) = value.parse::<f32>() {
                        state.set(key, F(literal));
                    } else {eprintln!("Unable to parse initial state {}'s value {}", key, value); return;}
                } else if let Ok(literal) = value.parse::<i32>() {
                    state.set(key, I(literal));
                } else { match value.as_str() {
                    "true" => state.set(key, B(true)),
                    "false" => state.set(key, B(false)),
                    _ => {eprintln!("Unable to parse initial state {}'s value {}", key, value); return;},
                }}
            } else {
                eprintln!("Usage: {} path/to/htn [initial_state_key initial_state_value [k v]]", args[0]);
                return;
            }
            pos += 2;
        }

        let plan = planner.plan(&state).unwrap();
        println!("Planer finished successfully. Plan: {:?}\nDecompiled plan:", plan);
        let blackboard = planner.domain.blackboard_mapping();
        let operators = planner.domain.operator_mapping();
        let mut stack = Vec::new();
        for op in plan.0 {
            match op {
                htn::compiler::Operation::ReadBlackboard(idx) => stack.push(blackboard[&idx]),
                htn::compiler::Operation::WriteBlackboard(idx) => println!("^ -> Store into {}", blackboard[&idx]),
                htn::compiler::Operation::CallOperator(idx, arity) => println!("{}({})", operators[idx], {
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
}


// first slide: 