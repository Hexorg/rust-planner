
use std::{collections::HashMap};

use super::{Operation, OperandType, optimization::Inertia};

#[derive(Debug, Eq)]
pub struct State<'a>{
    mapping: &'a HashMap<String, usize>,
    data: Vec<OperandType>,
}

impl std::clone::Clone for State<'_> {
    fn clone(&self) -> Self {
        Self { mapping: self.mapping, data: self.data.clone() }
    }
}

impl std::ops::Index<usize> for State<'_> {
    type Output = OperandType;

    fn index(&self, index: usize) -> &Self::Output {
        &self.data[index]
    }
}

impl std::ops::IndexMut<usize> for State<'_> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.data[index]
    }
}

impl std::hash::Hash for State<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.data.hash(state);
    }
}

impl std::cmp::PartialEq for State<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.mapping, other.mapping) && self.data == other.data
    }
}

impl std::cmp::PartialOrd for State<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if !std::ptr::eq(self.mapping, other.mapping) {
            return None
        }
        self.data.partial_cmp(&other.data)
    }
}

impl<'a> State<'a> {
    pub fn new(mapping:&'a HashMap<String, usize>) -> Self {
        let capacity = mapping.len();
        Self{
            mapping,
            data: vec![OperandType::default(); capacity]
        }
    }
    #[inline]
    pub fn get(&self, key:&str) -> OperandType {
        self.data[self.mapping[key]]
    }
    #[inline]
    pub fn set(&mut self, key:&str, value:OperandType) {
        self.data[self.mapping[key]] = value
    }

    pub fn eval(&self, bytecode:&[Operation]) -> Option<OperandType> {
        let mut stack = Vec::new();
        for op in bytecode {
            match op {
                Operation::ReadState(idx) => stack.push(self.data[*idx]),
                Operation::WriteState(_) => return None,
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

    pub fn eval_mut(&mut self, bytecode:&[Operation]) -> Option<OperandType> {
        let mut stack = Vec::new();
        for op in bytecode {
            match op {
                Operation::ReadState(idx) => stack.push(self.data[*idx]),
                Operation::WriteState(idx) => self.data[*idx] = stack.pop().unwrap(),
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
                Inertia::SmallerOrEquals(lit) => self.data[*k] = *lit,
                Inertia::NotItem(lit) => match lit {
                    OperandType::I(i) => self.data[*k] = OperandType::I(i+1),
                    OperandType::F(f) => self.data[*k] = OperandType::F(f+1.0),
                    OperandType::B(b) => self.data[*k] = OperandType::B(!b),
                },
                Inertia::Greater(lit) => match lit {
                    OperandType::I(i) => self.data[*k] = OperandType::I(i+1),
                    OperandType::F(f) => self.data[*k] = OperandType::F(f+1.0),
                    OperandType::B(_) => self.data[*k] = OperandType::B(true),
                },
                Inertia::Smaller(lit) => match lit {
                    OperandType::I(i) => self.data[*k] = OperandType::I(i-1),
                    OperandType::F(f) => self.data[*k] = OperandType::F(f-1.0),
                    OperandType::B(_) => self.data[*k] = OperandType::B(false),
                }
                Inertia::Depends(op, idx) => match op {
                    Operation::Equals => self.data[*k] = self.data[idx.unwrap()],
                    Operation::Greater => self.data[*k] = match self.data[idx.unwrap()] {
                        OperandType::I(i) => OperandType::I(i+1),
                        OperandType::F(f) => OperandType::F(f+1.0),
                        OperandType::B(_) => OperandType::B(true),
                    },
                    Operation::Smaller => self.data[*k] = match self.data[idx.unwrap()] {
                        OperandType::I(i) => OperandType::I(i-1),
                        OperandType::F(f) => OperandType::F(f-1.0),
                        OperandType::B(_) => OperandType::B(false),
                    },
                    Operation::GreaterOrEquals => self.data[*k] = self.data[idx.unwrap()],
                    Operation::SmallerOrEquals => self.data[*k] = self.data[idx.unwrap()],
                    Operation::Not => self.data[*k] = !self.data[idx.unwrap()],
                    Operation::And => todo!(),
                    Operation::Or => self.data[*k] = self.data[idx.unwrap()],
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
        for i in 0..self.data.len() {
            distance += match (self.data[i], other.data[i]) {
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



