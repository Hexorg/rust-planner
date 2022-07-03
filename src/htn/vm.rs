
use std::collections::HashMap;

use super::{domain::{Operation, OperandType}, optimization::Inertia};

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
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(OperandType::B(left == right))},
                Operation::Greater => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(OperandType::B(left > right))},
                Operation::Smaller => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(OperandType::B(left < right))},
                Operation::GreaterOrEquals => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(OperandType::B(left >= right))},
                Operation::SmallerOrEquals => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(OperandType::B(left <= right))},
                Operation::Not => {
                    let left = stack.pop().unwrap();
                    stack.push(!left)},
                Operation::And => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(left & right)},
                Operation::Or => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(left | right)},
                Operation::Subtract => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(left - right)},
                Operation::Add => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(left + right)},
                Operation::Multiply => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(left * right)},
                Operation::Divide => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(left / right)},
                Operation::ReadBlackboard(_) |
                Operation::WriteBlackboard(_) |
                Operation::PlanTask(_) |
                Operation::CallOperator(_, _) |
                Operation::OrNot => (),
            }
        }
        stack.pop()
    }

    pub fn admit(&mut self, wants:&HashMap<usize, Inertia>) {
        for (k, v) in wants {
            match v {
                Inertia::Item(lit) |
                Inertia::GreaterOrEquals(lit) |
                Inertia::SmallerOrEquals(lit) => self.set(*k, *lit),
                Inertia::NotItem(lit) => match lit {
                    OperandType::I(i) => self.set(*k, OperandType::I(i+1)),
                    OperandType::F(f) => self.set(*k, OperandType::F(f+1.0)),
                    OperandType::B(b) => self.set(*k, OperandType::B(!b)),
                },
                Inertia::Greater(lit) => match lit {
                    OperandType::I(i) => self.set(*k, OperandType::I(i+1)),
                    OperandType::F(f) => self.set(*k, OperandType::F(f+1.0)),
                    OperandType::B(_) => self.set(*k, OperandType::B(true)),
                },
                Inertia::Smaller(lit) => match lit {
                    OperandType::I(i) => self.set(*k, OperandType::I(i-1)),
                    OperandType::F(f) => self.set(*k, OperandType::F(f-1.0)),
                    OperandType::B(_) => self.set(*k, OperandType::B(false)),
                }
                Inertia::Depends(op, idx) => match op {
                    Operation::Equals => self.set(*k, self.get(idx.unwrap())),
                    Operation::Greater => self.set(*k, match self.get(idx.unwrap()) {
                        OperandType::I(i) => OperandType::I(i+1),
                        OperandType::F(f) => OperandType::F(f+1.0),
                        OperandType::B(_) => OperandType::B(true),
                    }),
                    Operation::Smaller => self.set(*k, match self.get(idx.unwrap()) {
                        OperandType::I(i) => OperandType::I(i-1),
                        OperandType::F(f) => OperandType::F(f-1.0),
                        OperandType::B(_) => OperandType::B(false),
                    }),
                    Operation::GreaterOrEquals => self.set(*k, self.get(idx.unwrap())),
                    Operation::SmallerOrEquals => self.set(*k, self.get(idx.unwrap())),
                    Operation::Not => self.set(*k, !self.get(idx.unwrap())),
                    Operation::And => todo!(),
                    Operation::Or => self.set(*k, self.get(idx.unwrap())),
                    Operation::Subtract => todo!(),
                    Operation::Add => todo!(),
                    Operation::Multiply => todo!(),
                    Operation::Divide => todo!(),
                    _ => (),
                },
                Inertia::Fluent => (),
                Inertia::Some => (),
                Inertia::None => (),
            }
        }
    }

    pub fn manhattan_distance(&self, other:&State) -> i32 {
        let mut distance = 0;
        for i in 0..self.0.len() {
            distance += match (self.get(i), other.get(i)) {
                (OperandType::I(l), OperandType::I(r)) => (l-r).abs(),
                (OperandType::I(i), OperandType::F(f)) |
                (OperandType::F(f), OperandType::I(i)) => 10*(i - f.floor() as i32).abs(),
                (OperandType::I(i), OperandType::B(b)) |
                (OperandType::B(b), OperandType::I(i))=> (i - if b { 1 } else { 0 }).abs(),

                (OperandType::F(l), OperandType::F(r)) => (l - r).abs().floor() as i32,
                (OperandType::F(_), OperandType::B(_)) |
                (OperandType::B(_), OperandType::F(_)) => i32::MAX,
                (OperandType::B(l), OperandType::B(r)) => if l ^ r { 1 } else { 0 },
            }
        }
        distance
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


