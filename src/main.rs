
mod htn;
use htn::domain::{Domain};


fn main() {
    let domain = Domain::from_file("htn-problems/testing.htn").expect("Domain construction error");

}
