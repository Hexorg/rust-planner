use std::fmt;
use super::expression::Expression;

pub struct Problem<'a> {
    pub objects: Vec<String>,
    pub initial_state: Expression<'a>,
    pub goal: Expression<'a>
}

impl fmt::Display for Problem<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use super::utils::build_var_string;
        let objects = self.objects.iter().fold(String::new(), |acc,item| acc + " ?" + item);
        fn rec_exp_to_str(e: &Expression, objects: &Vec<String>) -> String {
            match e {
                Expression::And(v) => format!("(and {})", v.iter().fold(String::new(), |acc,item| acc + " " + &rec_exp_to_str(item, objects))),
                Expression::Or(v) => format!("(or {})", v.iter().fold(String::new(), |acc,item| acc + " " + &rec_exp_to_str(item, objects))),
                Expression::Not(item) => format!("(not {})", rec_exp_to_str(item, objects)),
                Expression::Predicate(p, v) => format!("({} {})", p.name, build_var_string(v, Some(objects)))
            }
        }
        write!(f, "(define (problem problem)\n(:objects {})\n(:init {})\n(:goal {})\n)", objects, rec_exp_to_str(&self.initial_state, &self.objects), rec_exp_to_str(&self.goal, &self.objects))
    }
}