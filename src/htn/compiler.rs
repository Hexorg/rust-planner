
use std::collections::HashMap;

use super::parser::{self, tokens::{Token, TokenData, Literal}};

#[derive(Debug)]
pub enum Error {
    Parser(Option<String>, parser::Error),
    Basic(String, String),
    FromFile(String, Vec<Error>)
}

impl From<parser::Error> for Error {
    fn from(e: parser::Error) -> Self {
        Self::Parser(None, e)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Basic(s, e) => write!(f, "{}: {}", s, e),
            Self::FromFile(s, es) => {write!(f, "In file included from {}:\n", s)?; es.iter().try_for_each(|e| write!(f, "{}", e))},
            Self::Parser(None, e) => write!(f, "{:?}", e),
            Self::Parser(Some(filepath), e) => {
                let htn_source = std::fs::read_to_string(filepath).unwrap();
                let mut lines = htn_source.lines();
                if let Some(eline) = lines.nth(e.line - 1) {
                    let line_number_string = format!("{}", e.line);
                    writeln!(f, "{}:{} Error:", filepath, e.line)?; 
                    writeln!(f, "\t{}: {}", line_number_string, eline)?;
                    let debug_str_col_pos = line_number_string.len() + 1 + e.col;
                    writeln!(f, "\t{:->width$} {}\n",'^', e.message, width=debug_str_col_pos)?; 
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for Error {}

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
    Composite(Vec<usize>)
}

#[derive(Debug, PartialEq)]
pub struct Task {
    pub preconditions: Vec<Operation>,
    pub cost: Vec<Operation>,
    pub body: TaskBody,
    pub effects: Vec<Operation>,
    pub planning: Vec<Operation>,
    pub is_method: bool,
    wants: HashMap<usize, optimization::Inertia>,
    provides: HashMap<usize, optimization::Inertia>,
}

impl Task {
    pub fn wants(&self) -> &HashMap<usize, optimization::Inertia> {
        &self.wants
    }

    pub fn provides(&self) -> &HashMap<usize, optimization::Inertia> {
        &self.provides
    }
}

impl Default for Task {
    fn default() -> Self {
        Self { preconditions: Default::default(), cost: Default::default(), body: TaskBody::Primitive(Default::default()), effects: Default::default(), planning: Default::default(), is_method: Default::default(), wants: Default::default(), provides: Default::default() }
    }
}

#[inline]
fn get_varpath_idx(substitution:Option<(&str, &str)>, var_path:&[Token], mapping:&mut HashMap<String, usize>) -> usize {
    let vname = varpath_to_string(substitution, var_path);
    if mapping.contains_key(vname.as_str()) {
        mapping[vname.as_str()]
    } else {
        let s = mapping.len();
        mapping.insert(vname, s);
        s
    }
}

#[inline]
fn varpath_to_string(substitution:Option<(&str, &str)>, var_path:&[Token]) -> String {
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
    iter.map(|t| t.unwrap_identifier()).fold(first, |acc,item| acc + "." + item)
}

mod state_ops;
mod optimization;
pub mod state;
pub mod domain;