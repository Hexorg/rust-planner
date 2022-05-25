mod htn;
use htn::domain::{Domain};
use htn::planner::Planner;

fn main() {
    
    let domain = Domain::from_file("htn-problems/testing.htn").expect("Domain construction error");
    let plan = Planner::run(&domain);
    match plan {
        Ok(plan) => println!("Plan: {:?}", plan),
        Err(e) => domain.print_parse_error(&e),
    }
}
