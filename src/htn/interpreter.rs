
use crate::htn::parser::LabelToken;

use super::parser::{self, Expr, Error, TokenData};

#[derive(Clone, Debug, std::hash::Hash, std::cmp::PartialEq, std::cmp::PartialOrd, std::cmp::Eq, std::cmp::Ord)]
pub struct State<T>(pub Vec<T>);


impl<T:Copy + Default> State<T> {
    pub fn new(capacity:usize) -> Self {
        Self(vec![T::default(); capacity])
    }
    #[inline]
    pub fn get(&self, key:usize) -> T {
        self.0[key]
    }
    #[inline]
    pub fn set(&mut self, key:usize, value:T) {
        self.0[key] = value
    }
}


pub trait Evaluatable<T> where T: Copy + 
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
    fn eval_mut(&self, state:&mut State<T>) -> Result<(), Error>;
}

impl<T> Evaluatable<T> for Expr where T: Copy + Default +
        std::cmp::PartialEq +
        std::cmp::PartialOrd +
        std::convert::From<bool> + 
        std::convert::From<parser::Literal> + 
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
                    EqualsEquals => Ok((left == right).into()),
                    Minus => Ok(left - right),
                    Plus => Ok(left + right),
                    Slash => Ok(left / right),
                    Star => Ok(left * right),
                    Greater => Ok((left > right).into()),
                    Smaller => Ok((left < right).into()),
                    GreaterOrEquals => Ok((left >= right).into()),
                    SmallerOrEquals => Ok((left <= right).into()),
                    NotEquals => Ok((left != right).into()),
                    Or => Ok(left | right),
                    And => Ok(left & right),
                    _ => Err(self.to_err(String::from("Unsupported operation."))),
                }
            },
            Self::Grouping(g, _) => Ok(g.eval(state)?),
            Self::Literal(val, _) => Ok((*val).into()),
            Self::Variable(LabelToken{idx:Some(var),..}) => Ok(state.get(*var)),
            Self::Variable(LabelToken{idx:None,..}) => Err(self.to_err(String::from("Variable has not been converted to a state index"))),
            Self::Unary(op, right) => if let Not = op.t { Ok(!right.eval(state)?)  } else { 
                Err(self.to_err(String::from("Unexpected unary operator.")))
            },
            Self::Call(_, _) | 
            Self::Assignment(_,_) |
            Self::Noop(_) => Err(self.to_err(String::from("Unable to evaluate this expression."))),
        }
    }

    fn eval_mut(&self, state:&mut State<T>) -> Result<(), Error> {
        match self {
            Self::Assignment(LabelToken{idx:Some(var),..}, right) => {let r = right.eval(state)?; state.set(*var, r); Ok(())},
            Self::Noop(_) => Ok(()),
            _ => Err(self.to_err(String::from("This expression is not an assignment.")))
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum StateType {
    I(i32),
    B(bool),
    F(f32)
}

impl std::hash::Hash for StateType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

impl std::fmt::Display for StateType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StateType::I(i) => write!(f, "{}", i),
            StateType::B(b) => write!(f, "{}", b),
            StateType::F(d) => write!(f, "{}", d),
        }
    }
}


