
use std::collections::HashMap;
use std::hash::Hash;
use std::process::id;

use super::super::parser::pddl::ast::{self, TypedList, Predicate};
use super::Problem;
struct Compiler<'a> {
    domain:ast::Domain<'a>, 
    problem:ast::Problem<'a>,
    /// Map of object name or child type to the index of domain.types it's defined on.
    types: HashMap<&'a str, usize>,
    predicate_offset: HashMap<&'a str, usize>,
}

impl<'a> Compiler<'a> {
    fn build_type_map(&mut self) {
        for (idx, TypedList { identifiers, kind }) in self.domain.types.iter().enumerate() {
            for identifier in identifiers {
                self.types.insert(*identifier, idx);
            }
        }
        for TypedList { identifiers, kind } in &self.problem.objects {
            if self.types.contains_key(*kind) {
                for identifier in identifiers {
                    self.types.insert(*identifier, *self.types.get(kind).unwrap());
                }
            }
        }
    }

    fn is_type(&self, start_kind:&str, end_kind:&str) -> bool {
        if start_kind == end_kind {
            return true;
        }
        let mut current_parent = start_kind;
        while let Some(current_lvl) = self.types.get(current_parent) {
            current_parent = self.domain.types.get(*current_lvl).unwrap().kind;
            if current_parent == end_kind {
                return true;
            }
        }
        return false;
    }

    fn object_count(&self, object_kind:&str) -> usize {
        let mut result = 0;
        for TypedList { identifiers, kind } in &self.problem.objects {
            if self.is_type(kind, object_kind) {
                result += identifiers.len();
            }
        }
        result
    }

    fn build_state_space(&mut self) {
        let mut total_bits = 0;
        fn binom(n:usize,k:usize)->usize {
            let mut res = 1;
            for i in 0..k {
                res = (res * (n-i))/(i+1);
            }
            res
        }
        for Predicate { name, variables } in &self.domain.predicates {
            self.predicate_offset.insert(*name, total_bits);
            let mut predicate_bits = 1;
            for TypedList { identifiers, kind } in variables {
                predicate_bits *= binom(self.object_count(kind), identifiers.len());
            }
            total_bits += predicate_bits;
            println!("Predicate {} is represented with {} bits.", name, predicate_bits);
        }
        println!("Problem space: {} bits.", total_bits);
    }

    pub fn new(domain:ast::Domain<'a>, problem:ast::Problem<'a>) -> Self {
        let types = HashMap::new();
        let predicate_offset = HashMap::new();
        let mut r = Self {domain, problem, types, predicate_offset};
        r.build_type_map();
        r.build_state_space();
        r
    }
}

#[cfg(test)]
mod tests {
    use crate::htn::parser::pddl::Parser;
    use super::Compiler;

    #[test]
    fn test() {
        let domain_code = std::fs::read_to_string("pddl-problems/barman/domain.pddl").unwrap();
        let domain = Parser::new(domain_code.as_str()).next().unwrap().unwrap().unwrap_domain();
        let problem_code = std::fs::read_to_string("pddl-problems/barman/problem_5_10_7.pddl").unwrap();
        let problem = Parser::new(problem_code.as_str()).next().unwrap().unwrap().unwrap_problem();
        let c = Compiler::new(domain, problem);
        assert_eq!(c.object_count("container"), 8);
        assert_eq!(c.object_count("beverage"), 15);
    }

}