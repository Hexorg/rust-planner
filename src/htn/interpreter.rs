use std::{collections::HashMap, rc::Rc};
use super::parser::{self, Expr, Error, TokenData};

#[derive(Clone, Debug)]
pub struct State<T: std::hash::Hash>(pub HashMap<Rc<String>, T>);

impl<T> std::hash::Hash for State<T> where T:std::hash::Hash {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.iter().for_each(|(key, val)| {key.hash(state); val.hash(state);})
    }
}
impl<T> std::cmp::PartialEq for State<T>  where T:std::hash::Hash + std::cmp::PartialEq + std::fmt::Debug {
    fn eq(&self, other: &Self) -> bool {
        let mut result = true;
        self.0.iter().fold(&mut result, |acc, (k, v)| {*acc &=  other.0.contains_key(k) && other.0[k] == *v; acc});
        // println!("\tCMP: {:?} == {:?} ? {}", self.0, other.0, result);
        result
    }
}
impl<T> std::cmp::Eq for State<T> where T:std::hash::Hash + std::cmp::PartialEq + std::fmt::Debug { }


trait AsRC { fn asRC(&self) -> Rc<String>; }
impl AsRC for Rc<String> {
    fn asRC(&self) -> Rc<String> {
        return self.clone()
    }
}
impl AsRC for String {
    fn asRC(&self) -> Rc<String> {
        return Rc::new(self.clone())
    }
}
impl AsRC for &str {
    fn asRC(&self) -> Rc<String> {
        return Rc::new(String::from(*self))
    }
}
impl<T:std::hash::Hash> State<T> {
    #[inline]
    fn get(&self, key:&Rc<String>) -> Option<&T> {
        self.0.get(key)
    }
    #[inline]
    fn insert<V:AsRC>(&mut self, key:V, value:T) -> Option<T>{
        self.0.insert(key.asRC(), value)
    }
}
impl<T:std::hash::Hash> State<T> {
    pub fn new() -> State<T> {
        State(HashMap::new())
    }
}

pub trait Evaluatable<T> where T: Clone + 
        std::hash::Hash  + 
        std::cmp::PartialEq +
        std::cmp::PartialOrd +
        std::ops::Sub<Output = T> + 
        std::ops::Add<Output = T> + 
        std::ops::Div<Output = T> + 
        std::ops::Mul<Output = T> +
        std::ops::BitOr<Output = T> + 
        std::ops::BitAnd<Output = T> + 
        std::ops::Not<Output = T> {
    fn eval(&self, state:&State<T>) -> Result<T, Error> ;
    fn eval_mut(&self, state:&mut State<T>) -> Result<T, Error>;
}

impl<T> Evaluatable<T> for Expr where T: Clone + 
        std::hash::Hash + 
        std::cmp::PartialEq +
        std::cmp::PartialOrd +
        std::convert::From::<parser::Literal> +
        std::fmt::Display +
        std::ops::Sub<Output = T> + 
        std::ops::Add<Output = T> + 
        std::ops::Div<Output = T> + 
        std::ops::Mul<Output = T> +
        std::ops::BitOr<Output = T> + 
        std::ops::BitAnd<Output = T> + 
        std::ops::Not<Output = T> {
    fn eval(&self, state:&State<T>) -> Result<T, Error> {
        use TokenData::*;
        match self {
            Self::Binary(left, op, right) => {
                let left = left.eval(state)?;
                let right = right.eval(state)?;
                match op.t {
                    EqualsEquals => Ok(parser::Literal::B(left == right).into()),
                    Minus => Ok(left - right),
                    Plus => Ok(left + right),
                    Slash => Ok(left / right),
                    Star => Ok(left * right),
                    Greater => Ok(parser::Literal::B(left > right).into()),
                    Smaller => Ok(parser::Literal::B(left < right).into()),
                    GreaterOrEquals => Ok(parser::Literal::B(left >= right).into()),
                    SmallerOrEquals => Ok(parser::Literal::B(left <= right).into()),
                    NotEquals => Ok(parser::Literal::B(left != right).into()),
                    Or => Ok(left | right),
                    And => Ok(left & right),
                    _ => Err(self.to_err(String::from("Unsupported operation."))),
                }
            },
            Self::Grouping(g, _) => Ok(g.eval(state)?),
            Self::Literal(val, _) => Ok((*val).into()),
            Self::Variable(var, tok) => if let Some(val) = state.get(var) {
                Ok(val.clone())
            } else {
                Err(self.to_err(format!("Unknown variable name: {}.", var)))
            },
            Self::Unary(op, right) => if let Not = op.t { Ok(!right.eval(state)?)  } else { 
                Err(self.to_err(String::from("Unexpected unary operator.")))
            },
            Expr::Call(_, tok, _) | 
            Expr::Assignment(_,_,tok) |
            Expr::Noop(tok) => Err(self.to_err(String::from("Unable to evaluate this expression."))),
            
        }
    }

    fn eval_mut(&self, state:&mut State<T>) -> Result<T, Error> {
        match self {
            Self::Assignment(var, right, _) => {let r = right.eval(state)?; state.insert(var.clone(), r.clone()); Ok(r)},
            _ => self.eval(state)
        }
    }
}
