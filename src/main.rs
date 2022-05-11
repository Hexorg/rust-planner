
mod htn;
use htn::domain::{Domain};
use std::fs;

fn main() {
    let htn_source = fs::read_to_string("htn-problems/testing.htn")
        .expect("Error while opening .htn file");
    let domain = htn_source.parse::<Domain>().expect("Error while parsing domain");

}
