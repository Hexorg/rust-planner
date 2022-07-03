
use std::collections::HashMap;

use super::parser::{Error, tokens::{Token, TokenData, Literal}};
// use super::domain;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum OperandType {
    I(i32),
    F(f32),
    B(bool)
}

impl std::cmp::Eq for OperandType {

}

impl OperandType {
    #[inline]
    pub fn is_true(&self) -> bool {
        match self {
            Self::I(i) => *i == 1,
            Self::B(b) => *b,
            Self::F(_) => false
        }
    }
}

impl Default for OperandType {
    fn default() -> Self {
        Self::I(0)
    }
}

impl std::hash::Hash for OperandType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            OperandType::I(i) => i.hash(state),
            OperandType::F(f) => f.to_bits().hash(state),
            OperandType::B(b) => b.hash(state),
        }
    }
}

impl std::convert::TryFrom<Token<'_>> for OperandType {
    type Error = Error;
    fn try_from(value: Token) -> Result<Self, Self::Error> {
        use self::Literal::*;
        use TokenData::*;
        match value.t {
            Literal(I(i)) => Ok(OperandType::I(i)),
            Literal(F(f)) => Ok(OperandType::F(f)),
            Literal(B(b)) => Ok(OperandType::B(b)),
            Literal(S(_)) => Err(value.to_err("Unexpected string literal.").into()),
            _ => Err(value.to_err("Unexpected non-literal token.").into()),
        }
    }
}

impl std::ops::Add for OperandType {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        use OperandType::*;
        match (self, rhs) {
            (I(i), I(ri)) => I(i+ri),
            (F(f), F(rf)) => F(f+rf),
            _ => panic!("Attempt to subtract unsupported types"),
        }
    }
}

impl std::ops::Sub for OperandType {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        use OperandType::*;
        match (self, rhs) {
            (I(i), I(ri)) => I(i-ri),
            (F(f), F(rf)) => F(f-rf),
            _ => panic!("Attempt to subtract unsupported types"),
        }
    }
}

impl std::ops::Mul for OperandType {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        use OperandType::*;
        match (self, rhs) {
            (OperandType::I(i), OperandType::I(ri)) => I(i*ri),
            (OperandType::F(f), OperandType::F(rf)) => F(f*rf),
            _ => panic!("Attempt to subtract unsupported types"),
        }
    }
}

impl std::ops::Div for OperandType {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        use OperandType::*;
        match (self, rhs) {
            (OperandType::I(i), OperandType::I(ri)) => I(i/ri),
            (OperandType::F(f), OperandType::F(rf)) => F(f/rf),
            _ => panic!("Attempt to subtract unsupported types"),
        }
    }
}

impl std::ops::BitOr for OperandType {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        use OperandType::*;
        match (self, rhs) {
            (I(i), I(ir)) => I(i | ir),
            (I(i), B(b)) |
            (B(b), I(i)) => I(i | if b { 1 } else { 0 }),
            (B(b), B(br)) => B(b || br),
            _ => panic!("Attempt to Or unsupported types"),
        }
    }
}

impl std::ops::BitAnd for OperandType {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        use OperandType::*;
        match (self, rhs) {
            (I(i), I(ir)) => I(i & ir),
            (I(i), B(b)) |
            (B(b), I(i)) => if b { B(i & 1 > 0) } else { B(b) },
            (B(b), B(br)) => B(b && br),
            _ => panic!("Attempt to And unsupported types"),
        }
    }
}

impl std::ops::Not for OperandType {
    type Output = Self;

    fn not(self) -> Self::Output {
        use OperandType::*;
        match self {
            B(b) => B(!b),
            _ => panic!("Attempt to NOT unsupported types"),
        }
    }
}


#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operation {
    ReadState(usize),
    WriteState(usize),
    Push(OperandType),
    Equals,
    Greater,
    Smaller,
    GreaterOrEquals,
    SmallerOrEquals,
    Not, 
    And,
    Or,
    OrNot, // used in inertia calculation only
    Subtract,
    Add,
    Multiply,
    Divide,
    ReadBlackboard(usize),
    WriteBlackboard(usize),
    PlanTask(usize),
    CallOperator(usize, usize), // (operator_id, arity)
}

#[derive(Debug, PartialEq)]
pub enum TaskBody {
    Primitive(Vec<Operation>),
    Composite(Vec<Task>)
}

#[derive(Debug, PartialEq)]
pub struct Task {
    pub preconditions: Vec<Operation>,
    pub cost: Vec<Operation>,
    pub body: TaskBody,
    pub effects: Vec<Operation>,
    pub planning: Vec<Operation>,
    wants: HashMap<usize, optimization::Inertia>,
    provides: HashMap<usize, optimization::Inertia>,
}

#[inline]
fn get_varpath_idx(substitution:Option<(&str, &str)>, var_path:&[Token], mapping:&mut HashMap<String, usize>) -> Result<usize, Error> {
    let mut iter = var_path.iter();
    let first = iter.by_ref().take(1).map(|t| t.unwrap_identifier()).fold(String::new(), |acc, item| {
        acc + if let Some((from, to)) = substitution {
            if item == from {
                to
            } else {
                item
            }
        } else {
            item
        }
    });
    let vname = iter.map(|t| t.unwrap_identifier()).fold(first, |acc,item| acc + "." + item);
    if mapping.contains_key(vname.as_str()) {
        Ok(mapping[vname.as_str()])
    } else {
        let s = mapping.len();
        mapping.insert(vname, s);
        Ok(s)
    }
}

mod state_ops;
mod optimization;
pub mod domain;