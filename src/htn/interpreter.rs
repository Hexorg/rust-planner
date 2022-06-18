
use super::parser::{self, expression::Expr, Error, tokens::{TokenData, Literal}};

#[derive(Clone, Debug, std::hash::Hash, std::cmp::PartialEq, std::cmp::PartialOrd, std::cmp::Eq, std::cmp::Ord)]
pub struct State<T>(Vec<T>);


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

impl<T:std::ops::Sub<Output = T> + Default + std::convert::Into<i32>+Copy> State<T> {
    fn manhattan(&self, other:&Self) -> i32 {
        self.0.iter().zip(other.0.iter()).fold(T::default(), |acc, (l, r)| *l-*r).into()
    }
}

// impl<T:std::hash::Hash> std::hash::Hash for State<T> {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         self.0.iter().for_each(|i|i.hash(state));
//     }
// }

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
        std::convert::From<Literal> + 
        std::ops::Sub<Output = T> + 
        std::ops::Add<Output = T> + 
        std::ops::Div<Output = T> + 
        std::ops::Mul<Output = T> +
        std::ops::BitOr<Output = T> + 
        std::ops::BitAnd<Output = T> + 
        std::ops::Not<Output = T> {
    fn eval(&self, state:&State<T>) -> Result<T, Error> {
        todo!();
        use TokenData::*;
        // match self {
        //     Self::Binary(left, op, right) => {
        //         let left = left.eval(state)?;
        //         let right = right.eval(state)?;
        //         match op.t {
        //             EqualsEquals => Ok((left == right).into()),
        //             Minus => Ok(left - right),
        //             Plus => Ok(left + right),
        //             Slash => Ok(left / right),
        //             Star => Ok(left * right),
        //             Greater => Ok((left > right).into()),
        //             Smaller => Ok((left < right).into()),
        //             GreaterOrEquals => Ok((left >= right).into()),
        //             SmallerOrEquals => Ok((left <= right).into()),
        //             NotEquals => Ok((left != right).into()),
        //             Or => Ok(left | right),
        //             And => Ok(left & right),
        //             _ => Err(self.to_err(String::from("Unsupported operation."))),
        //         }
        //     },
        //     Self::Grouping(g, _) => Ok(g.eval(state)?),
        //     Self::Literal(val, _) => Ok((val.clone()).into()),
        //     Self::Variable(LabelToken{idx:Some(var),..}) => Ok(state.get(*var)),
        //     Self::Variable(LabelToken{idx:None,..}) => Err(self.to_err(String::from("Variable has not been converted to a state index"))),
        //     Self::Unary(op, right) => if let Not = op.t { Ok(!right.eval(state)?)  } else { 
        //         Err(self.to_err(String::from("Unexpected unary operator.")))
        //     },
        //     Self::Call(_, _) | 
        //     Self::Assignment(_,_) |
        //     Self::Noop(_) => Err(self.to_err(String::from("Unable to evaluate this expression."))),
        // }
    }

    fn eval_mut(&self, state:&mut State<T>) -> Result<(), Error> {
        todo!();
        // match self {
        //     Self::Assignment(LabelToken{idx:Some(var),..}, right) => {let r = right.eval(state)?; state.set(*var, r); Ok(())},
        //     Self::Noop(_) => Ok(()),
        //     _ => Err(self.to_err(String::from("This expression is not an assignment.")))
        // }
    }

    
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StateType {
    I(i32),
    B(bool),
    F(f32)
}

impl std::hash::Hash for StateType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            StateType::I(i) => i.hash(state),
            StateType::B(b) => b.hash(state),
            StateType::F(f) => if f.is_normal() { f.to_bits().hash(state) } else { i32::MAX.hash(state)},
        }
    }
}

impl std::cmp::Eq for StateType {

}

impl std::cmp::PartialOrd for StateType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (StateType::I(i), StateType::I(oi)) => Some(i.cmp(oi)),
            (StateType::B(b), StateType::B(bo)) => Some(b.cmp(bo)),
            (StateType::F(f), StateType::F(fo)) if f.is_finite() && fo.is_finite() => f.partial_cmp(fo),
            _ => panic!("Can't compare different types: {:?} and {:?}", self, other)
        }
    }
}

impl std::cmp::Ord for StateType {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;
        match (self, other) {
            (StateType::I(i), StateType::I(oi)) => i.cmp(oi),
            (StateType::B(b), StateType::B(bo)) => b.cmp(bo),
            (StateType::F(f), StateType::F(fo)) if f.is_finite() && fo.is_finite() => f.partial_cmp(fo).unwrap(),
            _ => panic!("Can't compare different types")
        }
    }
}

impl std::default::Default for StateType {
    fn default() -> Self {
        Self::I(0)
    }
}

impl From<bool> for StateType {
    fn from(v: bool) -> Self {
        Self::B(v)
    }
}

impl From<Literal> for StateType {
    fn from(l: Literal) -> Self {
        match l {
            Literal::I(i) => Self::I(i),
            Literal::F(f) => Self::F(f),
            Literal::B(b) => Self::B(b),
            Literal::S(_) => panic!("String literals can not be part of state.")
        }
    }
}

impl std::ops::Sub for StateType {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        use StateType::*;
        match (self, rhs) {
            (StateType::I(i), StateType::I(ri)) => I(i-ri),
            (StateType::F(f), StateType::F(rf)) => F(f-rf),
            (StateType::B(b), StateType::B(rb)) => if b == rb { I(0) } else { I(1) }
            _ => panic!("Attempt to subtract unsupported types"),
        }
    }
}

impl std::ops::Add for StateType {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        use StateType::*;
        match (self, rhs) {
            (StateType::I(i), StateType::I(ri)) => I(i+ri),
            (StateType::F(f), StateType::F(rf)) => F(f+rf),
            _ => panic!("Attempt to subtract unsupported types"),
        }
    }
}

impl std::ops::Mul for StateType {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        use StateType::*;
        match (self, rhs) {
            (StateType::I(i), StateType::I(ri)) => I(i*ri),
            (StateType::F(f), StateType::F(rf)) => F(f*rf),
            _ => panic!("Attempt to subtract unsupported types"),
        }
    }
}

impl std::ops::Div for StateType {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        use StateType::*;
        match (self, rhs) {
            (StateType::I(i), StateType::I(ri)) => I(i/ri),
            (StateType::F(f), StateType::F(rf)) => F(f/rf),
            _ => panic!("Attempt to subtract unsupported types"),
        }
    }
}

impl std::ops::BitOr for StateType {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        use StateType::*;
        match (self, rhs) {
            (I(i), I(ir)) => I(i | ir),
            (I(i), B(b)) |
            (B(b), I(i)) => I(i | if b { 1 } else { 0 }),
            (B(b), B(br)) => B(b || br),
            _ => panic!("Attempt to Or unsupported types"),
        }
    }
}

impl std::ops::BitAnd for StateType {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        use StateType::*;
        match (self, rhs) {
            (I(i), I(ir)) => I(i & ir),
            (I(i), B(b)) |
            (B(b), I(i)) => if b { B(i & 1 > 0) } else { B(b) },
            (B(b), B(br)) => B(b && br),
            _ => panic!("Attempt to And unsupported types"),
        }
    }
}

impl std::ops::Not for StateType {
    type Output = Self;

    fn not(self) -> Self::Output {
        use StateType::*;
        match self {
            B(b) => B(!b),
            _ => panic!("Attempt to NOT unsupported types"),
        }
    }
}

impl std::convert::Into<i32> for StateType {
    fn into(self) -> i32 {
        match self {
            StateType::I(i) => i,
            StateType::B(b) => if b { 1 } else { 0 },
            StateType::F(f) => if f.is_normal() { unsafe { f.to_int_unchecked() } } else { i32::MAX },
        }
    }
}

impl std::convert::Into<bool> for StateType {
    fn into(self) -> bool {
        match self {
            StateType::I(i) => i == 1,
            StateType::B(b) => b,
            _ => panic!("Can not convert f32 to bool")
        }
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


