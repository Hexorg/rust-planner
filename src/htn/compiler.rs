
use super::parser::{Error, tokens::{Token, TokenData::*, Literal}};
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
        match value.t {
            Literal(Literal::I(i)) => Ok(OperandType::I(i)),
            Literal(Literal::F(f)) => Ok(OperandType::F(f)),
            Literal(Literal::B(b)) => Ok(OperandType::B(b)),
            Literal(Literal::S(_)) => Err(value.to_err("Unexpected string literal.").into()),
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


pub mod preconditions;