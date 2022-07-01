
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
pub mod domain;