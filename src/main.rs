mod htn;
use htn::domain::{Domain};
use htn::planner::Planner;

fn main() {
    
    let domain = Domain::from_file("htn-problems/testing.htn").expect("Domain construction error");
    Planner::run(&domain);

}
