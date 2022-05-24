use std::fmt;
use super::utils::build_var_string;



impl Predicate {
    pub fn new(name: &str, variable_count:usize) -> Predicate {
        Predicate {
            name:name.to_string(),
            variable_count
        }
    }
}

impl fmt::Display for Predicate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {})", self.name, build_var_string(&(0..self.variable_count).collect(), None))
    }
}