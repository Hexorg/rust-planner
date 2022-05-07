use std::fmt;
use std::collections::BTreeSet;
use super::predicate::Predicate;

#[derive(Clone)]
pub enum Expression<'a> {
    And(Vec<Expression<'a>>),
    Or(Vec<Expression<'a>>),
    Not(Box<Expression<'a>>),
    Predicate(&'a Predicate, Vec<usize>)
}

#[macro_export]
macro_rules! expNot{ 
    ( $e: expr ) => {
        Expression::Not(Box::new($e.clone()))
    };
}

#[macro_export]
macro_rules! expAnd {
    ( $($e: expr), * ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($e.clone());
            )*
            Expression::And(temp_vec)
        }
    };
}

#[macro_export]
macro_rules! expOr {
    ( $($e: expr), * ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($e.clone());
            )*
            Expression::Or(temp_vec)
        }
    };
}

impl fmt::Display for Expression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use super::utils::build_var_string;
        match self {
            Expression::Or(v) => { write!(f, "(or ")?; v.iter().fold(Ok(()), |_,item| write!(f, "{} ", item))?; write!(f, ")")},
            Expression::And(v) => { write!(f, "(and ")?; v.iter().fold(Ok(()), |_,item| write!(f, "{} ", item))?; write!(f, ")")},
            Expression::Not(item) => write!(f, "(not {})", item),
            Expression::Predicate(p, v) => write!(f, "({} {})", p.name, build_var_string(v, None))
        }
    }
}

impl<'a> Expression<'a> {
    pub fn variables(&self) -> BTreeSet<usize> {
        let mut variable_set = BTreeSet::new();
        fn rec_count(e: &Expression, set: &mut BTreeSet<usize>) {
            match e {
                Expression::Or(v) |
                Expression::And(v) => v.iter().for_each(|e| rec_count(e, set)),
                Expression::Not(e) => rec_count(e, set),
                Expression::Predicate(_,v) => set.extend(v),
            }
        }
        rec_count(&self, &mut variable_set);
        variable_set
    }
}


