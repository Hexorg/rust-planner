use std::fmt;
use super::expression::Expression;

pub struct Action<'a> {
    pub name: String,
    pub precondition: Expression<'a>,
    pub effect: Expression<'a>,
}

impl fmt::Display for Action<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use super::utils::build_var_string;
        let variables = self.precondition.variables().union(&self.effect.variables()).cloned().collect();
        write!(f, "(:action {} :parameters {} :precondition {} :effect {}",
            self.name, build_var_string(&variables, None), self.precondition, self.effect)
    }
}
