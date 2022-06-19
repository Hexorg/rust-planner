
use super::domain::{Operation, OperandType};

#[derive(Clone, Debug, std::hash::Hash, std::cmp::PartialEq, std::cmp::PartialOrd, std::cmp::Eq)]
pub struct State(Vec<OperandType>);

impl State {
    pub fn new(capacity:usize) -> Self {
        Self(vec![OperandType::default(); capacity])
    }
    #[inline]
    pub fn get(&self, key:usize) -> OperandType {
        self.0[key]
    }
    #[inline]
    pub fn set(&mut self, key:usize, value:OperandType) {
        self.0[key] = value
    }

    pub fn eval(&mut self, bytecode:&Vec<Operation>) -> Option<OperandType> {
        let mut stack = Vec::new();
        for op in bytecode {
            match op {
                Operation::ReadState(idx) => stack.push(self.0[*idx]),
                Operation::WriteState(idx) => self.0[*idx] = stack.pop().unwrap(),
                Operation::Push(v) => stack.push(*v),
                Operation::Equals => {
                    let left = stack.pop().unwrap();
                    let right = stack.pop().unwrap();
                    stack.push(OperandType::B(left == right))},
                Operation::Greater => {
                    let left = stack.pop().unwrap();
                    let right = stack.pop().unwrap();
                    stack.push(OperandType::B(left > right))},
                Operation::Smaller => {
                    let left = stack.pop().unwrap();
                    let right = stack.pop().unwrap();
                    stack.push(OperandType::B(left < right))},
                Operation::GreaterOrEquals => {
                    let left = stack.pop().unwrap();
                    let right = stack.pop().unwrap();
                    stack.push(OperandType::B(left >= right))},
                Operation::SmallerOrEquals => {
                    let left = stack.pop().unwrap();
                    let right = stack.pop().unwrap();
                    stack.push(OperandType::B(left <= right))},
                Operation::Not => {
                    let left = stack.pop().unwrap();
                    stack.push(!left)},
                Operation::And => {
                    let left = stack.pop().unwrap();
                    let right = stack.pop().unwrap();
                    stack.push(left & right)},
                Operation::Or => {
                    let left = stack.pop().unwrap();
                    let right = stack.pop().unwrap();
                    stack.push(left | right)},
                Operation::Subtract => {
                    let left = stack.pop().unwrap();
                    let right = stack.pop().unwrap();
                    stack.push(left - right)},
                Operation::Add => {
                    let left = stack.pop().unwrap();
                    let right = stack.pop().unwrap();
                    stack.push(left + right)},
                Operation::Multiply => {
                    let left = stack.pop().unwrap();
                    let right = stack.pop().unwrap();
                    stack.push(left * right)},
                Operation::Divide => {
                    let left = stack.pop().unwrap();
                    let right = stack.pop().unwrap();
                    stack.push(left / right)},
                Operation::ReadBlackboard(_) |
                Operation::WriteBlackboard(_) |
                Operation::PlanTask(_) |
                Operation::CallOperator(_, _) => (),
            }
        }
        stack.pop()
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

// impl std::convert::Into<i32> for OperandType {
//     fn into(self) -> i32 {
//         match self {
//             OperandType::I(i) => i,
//             OperandType::B(b) => if b { 1 } else { 0 },
//             OperandType::F(f) => if f.is_normal() { unsafe { f.to_int_unchecked() } } else { i32::MAX },
//         }
//     }
// }

// impl std::convert::Into<bool> for OperandType {
//     fn into(self) -> bool {
//         match self {
//             OperandType::I(i) => i == 1,
//             OperandType::B(b) => b,
//             _ => panic!("Can not convert f32 to bool")
//         }
//     }
// }


// impl std::fmt::Display for OperandType {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             OperandType::I(i) => write!(f, "{}", i),
//             OperandType::B(b) => write!(f, "{}", b),
//             OperandType::F(d) => write!(f, "{}", d),
//         }
//     }
// }


