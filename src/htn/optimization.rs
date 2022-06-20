use std::collections::HashMap;

use super::{parser::{expression::ExpressionVisitor, tokens::{Token, TokenData}}, domain::Operation};
use super::domain::{self, OperandType};

#[derive(Debug, Clone, Copy)]
pub enum Wants {
    Item(OperandType),
    // Items(Vec<OperandType>),
    NotItem(OperandType),
    // NotItems(Vec<OperandType>),
    Greater(OperandType),
    GreaterOrEquals(OperandType),
    Smaller(OperandType),
    SmallerOrEquals(OperandType),
    Depends(Operation, Option<usize>), // Depends on the variable
    Any, // no constraints, eg. preconditions are (true)
    Some, // Can't determine statically
    None // Can't satisfy constraints. preconditions are unreachable.
}

impl Wants {
    fn bool_eval(&self, op:&Operation, arg:OperandType) -> Result<Wants, domain::Error> {
        let inertia = match self {
            Self::Item(OperandType::B(v)) => *v,
            Self::NotItem(OperandType::B(v)) => !*v,
            Self::Any => true,
            _ => return Ok(Wants::None) // (? == 5) can never result in, e.g. 124
        };
        // println!("bool_eval({:?}, {:?} has {} inertia.", op, arg, inertia);
        use Operation::*;
        Ok(match (inertia, op) {
            (true, Equals) => Wants::Item(arg),
            (true, Smaller) => Wants::Smaller(arg),
            (true, SmallerOrEquals) => Wants::SmallerOrEquals(arg),
            (true, Greater) => Wants::Greater(arg),
            (true, GreaterOrEquals) => Wants::GreaterOrEquals(arg),
            (true, And) => if arg.is_true() { Wants::Any} else {Wants::None},

            
            (false, Equals) => Wants::NotItem(arg),
            (false, Smaller) => Wants::GreaterOrEquals(arg),
            (false, SmallerOrEquals) => Wants::Greater(arg),
            (false, Greater) => Wants::SmallerOrEquals(arg) ,
            (false, GreaterOrEquals) => Wants::Smaller(arg),
            (false, And) => if arg.is_true() { Wants::None} else {Wants::Any},
            (_, Not) => Wants::Item(OperandType::B(!inertia)),
            (_, Or) => if arg.is_true() { Wants::Any} else {Wants::Item(OperandType::B(inertia))},
            _ => todo!("More operations...?")
            // _ => return Err(domain::Error::Domain(String::new(), "Unexpected operation."))
        })
    }

    pub fn inverted(&self) -> Wants {
        match self {
            Wants::Item(i) => Wants::NotItem(*i),
            Wants::NotItem(i) => Wants::Item(*i),
            Wants::Greater(i) => Wants::SmallerOrEquals(*i),
            Wants::GreaterOrEquals(i) => Wants::Smaller(*i),
            Wants::Smaller(i) => Wants::GreaterOrEquals(*i),
            Wants::SmallerOrEquals(i) => Wants::Greater(*i),
            Wants::Depends(op, var) => match op {
                Operation::Equals => Wants::Depends(Operation::Not, *var),
                Operation::Greater => Wants::Depends(Operation::SmallerOrEquals, *var),
                Operation::Smaller => Wants::Depends(Operation::GreaterOrEquals, *var),
                Operation::GreaterOrEquals => Wants::Depends(Operation::Smaller, *var),
                Operation::SmallerOrEquals => Wants::Depends(Operation::Greater, *var),
                Operation::Not => Wants::Depends(Operation::Equals, *var),
                Operation::And => todo!(),
                Operation::Or => todo!(),
                Operation::Subtract => todo!(),
                Operation::Add => todo!(),
                Operation::Multiply => todo!(),
                Operation::Divide => todo!(),
                _ => panic!("Unsupported operation.")
            }
            Wants::Any => Wants::None,
            Wants::Some => Wants::Some,
            Wants::None => Wants::Any,
        }
    }

    pub fn intersection(&self, other:&Self, arg:OperandType) -> Result<Wants, domain::Error> {
        // println!("Getting intersection of {:?} and {:?} wants with arg {:?}.", self, other, arg);
        match (self, other) {
            (Wants::Item(_), Wants::Item(_)) => todo!(),
            (Wants::Item(_), Wants::NotItem(_)) => todo!(),
            (Wants::Item(_), Wants::Greater(_)) => todo!(),
            (Wants::Item(_), Wants::GreaterOrEquals(_)) => todo!(),
            (Wants::Item(_), Wants::Smaller(_)) => todo!(),
            (Wants::Item(_), Wants::SmallerOrEquals(_)) => todo!(),
            (Wants::Item(_), Wants::Depends(op, None)) => self.bool_eval(op, arg),
            (Wants::Item(_), Wants::Any) => todo!(),
            (Wants::Item(_), Wants::Some) => todo!(),
            (Wants::Item(_), Wants::None) => todo!(),
            (Wants::NotItem(_), Wants::Item(_)) => todo!(),
            (Wants::NotItem(_), Wants::NotItem(_)) => todo!(),
            (Wants::NotItem(_), Wants::Greater(_)) => todo!(),
            (Wants::NotItem(_), Wants::GreaterOrEquals(_)) => todo!(),
            (Wants::NotItem(_), Wants::Smaller(_)) => todo!(),
            (Wants::NotItem(_), Wants::SmallerOrEquals(_)) => todo!(),
            (Wants::NotItem(_), Wants::Depends(_, _)) => todo!(),
            (Wants::NotItem(_), Wants::Any) => todo!(),
            (Wants::NotItem(_), Wants::Some) => todo!(),
            (Wants::NotItem(_), Wants::None) => todo!(),
            (Wants::Greater(_), Wants::Item(_)) => todo!(),
            (Wants::Greater(_), Wants::NotItem(_)) => todo!(),
            (Wants::Greater(_), Wants::Greater(_)) => todo!(),
            (Wants::Greater(_), Wants::GreaterOrEquals(_)) => todo!(),
            (Wants::Greater(_), Wants::Smaller(_)) => todo!(),
            (Wants::Greater(_), Wants::SmallerOrEquals(_)) => todo!(),
            (Wants::Greater(_), Wants::Depends(_, _)) => todo!(),
            (Wants::Greater(_), Wants::Any) => todo!(),
            (Wants::Greater(_), Wants::Some) => todo!(),
            (Wants::Greater(_), Wants::None) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::Item(_)) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::NotItem(_)) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::Greater(_)) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::GreaterOrEquals(_)) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::Smaller(_)) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::SmallerOrEquals(_)) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::Depends(_, _)) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::Any) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::Some) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::None) => todo!(),
            (Wants::Smaller(_), Wants::Item(_)) => todo!(),
            (Wants::Smaller(_), Wants::NotItem(_)) => todo!(),
            (Wants::Smaller(_), Wants::Greater(_)) => todo!(),
            (Wants::Smaller(_), Wants::GreaterOrEquals(_)) => todo!(),
            (Wants::Smaller(_), Wants::Smaller(_)) => todo!(),
            (Wants::Smaller(_), Wants::SmallerOrEquals(_)) => todo!(),
            (Wants::Smaller(_), Wants::Depends(_, _)) => todo!(),
            (Wants::Smaller(_), Wants::Any) => todo!(),
            (Wants::Smaller(_), Wants::Some) => todo!(),
            (Wants::Smaller(_), Wants::None) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::Item(_)) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::NotItem(_)) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::Greater(_)) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::GreaterOrEquals(_)) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::Smaller(_)) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::SmallerOrEquals(_)) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::Depends(_, _)) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::Any) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::Some) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::None) => todo!(),
            (Wants::Depends(_, _), Wants::Item(_)) => todo!(),
            (Wants::Depends(_, _), Wants::NotItem(_)) => todo!(),
            (Wants::Depends(_, _), Wants::Greater(_)) => todo!(),
            (Wants::Depends(_, _), Wants::GreaterOrEquals(_)) => todo!(),
            (Wants::Depends(_, _), Wants::Smaller(_)) => todo!(),
            (Wants::Depends(_, _), Wants::SmallerOrEquals(_)) => todo!(),
            (Wants::Depends(_, _), Wants::Depends(_, _)) => todo!(),
            (Wants::Depends(_, _), Wants::Any) => todo!(),
            (Wants::Depends(_, _), Wants::Some) => todo!(),
            (Wants::Depends(_, _), Wants::None) => todo!(),
            (Wants::Any, Wants::Item(_)) => todo!(),
            (Wants::Any, Wants::NotItem(_)) => todo!(),
            (Wants::Any, Wants::Greater(_)) => todo!(),
            (Wants::Any, Wants::GreaterOrEquals(_)) => todo!(),
            (Wants::Any, Wants::Smaller(_)) => todo!(),
            (Wants::Any, Wants::SmallerOrEquals(_)) => todo!(),
            (Wants::Any, Wants::Depends(_, _)) => todo!(),
            (Wants::Any, Wants::Any) => todo!(),
            (Wants::Any, Wants::Some) => todo!(),
            (Wants::Any, Wants::None) => todo!(),
            (Wants::Some, Wants::Item(_)) => todo!(),
            (Wants::Some, Wants::NotItem(_)) => todo!(),
            (Wants::Some, Wants::Greater(_)) => todo!(),
            (Wants::Some, Wants::GreaterOrEquals(_)) => todo!(),
            (Wants::Some, Wants::Smaller(_)) => todo!(),
            (Wants::Some, Wants::SmallerOrEquals(_)) => todo!(),
            (Wants::Some, Wants::Depends(_, _)) => todo!(),
            (Wants::Some, Wants::Any) => todo!(),
            (Wants::Some, Wants::Some) => todo!(),
            (Wants::Some, Wants::None) => todo!(),
            (Wants::None, Wants::Item(_)) => todo!(),
            (Wants::None, Wants::NotItem(_)) => todo!(),
            (Wants::None, Wants::Greater(_)) => todo!(),
            (Wants::None, Wants::GreaterOrEquals(_)) => todo!(),
            (Wants::None, Wants::Smaller(_)) => todo!(),
            (Wants::None, Wants::SmallerOrEquals(_)) => todo!(),
            (Wants::None, Wants::Depends(_, _)) => todo!(),
            (Wants::None, Wants::Any) => todo!(),
            (Wants::None, Wants::Some) => todo!(),
            (Wants::None, Wants::None) => todo!(),
            _ => Err(domain::Error::Domain(String::new(), format!("Unexpected want combination: {:?} and {:?}.", self, other))),
        }
    }
    pub fn satisfies(&self, provides:&Wants) -> bool {
        match (self, provides) {
            (Wants::Item(want), Wants::Item(have)) => want == have,
            (Wants::Item(want), Wants::NotItem(have)) => want != have,
            (Wants::Item(want), Wants::Greater(have)) => want > have,
            (Wants::Item(want), Wants::GreaterOrEquals(have)) => want >= have,
            (Wants::Item(want), Wants::Smaller(have)) => want < have,
            (Wants::Item(want), Wants::SmallerOrEquals(have)) => want <= have,
            (_, Wants::Depends(_, _)) => true,
            (Wants::Item(want), Wants::Any) => todo!(),
            (Wants::Item(want), Wants::Some) => true,
            (Wants::Item(want), Wants::None) => false,
            (Wants::NotItem(want), Wants::Item(have)) => todo!(),
            (Wants::NotItem(want), Wants::NotItem(have)) => todo!(),
            (Wants::NotItem(want), Wants::Greater(have)) => todo!(),
            (Wants::NotItem(want), Wants::GreaterOrEquals(have)) => todo!(),
            (Wants::NotItem(want), Wants::Smaller(have)) => todo!(),
            (Wants::NotItem(want), Wants::SmallerOrEquals(have)) => todo!(),
            (Wants::NotItem(want), Wants::Any) => todo!(),
            (Wants::NotItem(want), Wants::Some) => todo!(),
            (Wants::NotItem(want), Wants::None) => todo!(),
            (Wants::Greater(want), Wants::Item(have)) => have > want,
            (Wants::Greater(want), Wants::NotItem(have)) => true,
            (Wants::Greater(want), Wants::Greater(have)) => true,
            (Wants::Greater(want), Wants::GreaterOrEquals(have)) => true,
            (Wants::Greater(want), Wants::Smaller(have)) => have < want,
            (Wants::Greater(want), Wants::SmallerOrEquals(have)) => have <= want,
            (Wants::Greater(want), Wants::Any) => true,
            (Wants::Greater(want), Wants::Some) => true,
            (Wants::Greater(want), Wants::None) => false,
            (Wants::GreaterOrEquals(_), Wants::Item(_)) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::NotItem(_)) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::Greater(_)) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::GreaterOrEquals(_)) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::Smaller(_)) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::SmallerOrEquals(_)) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::Any) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::Some) => todo!(),
            (Wants::GreaterOrEquals(_), Wants::None) => todo!(),
            (Wants::Smaller(_), Wants::Item(_)) => todo!(),
            (Wants::Smaller(_), Wants::NotItem(_)) => todo!(),
            (Wants::Smaller(_), Wants::Greater(_)) => todo!(),
            (Wants::Smaller(_), Wants::GreaterOrEquals(_)) => todo!(),
            (Wants::Smaller(_), Wants::Smaller(_)) => todo!(),
            (Wants::Smaller(_), Wants::SmallerOrEquals(_)) => todo!(),
            (Wants::Smaller(_), Wants::Any) => todo!(),
            (Wants::Smaller(_), Wants::Some) => todo!(),
            (Wants::Smaller(_), Wants::None) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::Item(_)) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::NotItem(_)) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::Greater(_)) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::GreaterOrEquals(_)) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::Smaller(_)) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::SmallerOrEquals(_)) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::Any) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::Some) => todo!(),
            (Wants::SmallerOrEquals(_), Wants::None) => todo!(),
            (Wants::Depends(_, _), _) => true,
            (Wants::Any, Wants::Item(_)) => todo!(),
            (Wants::Any, Wants::NotItem(_)) => todo!(),
            (Wants::Any, Wants::Greater(_)) => todo!(),
            (Wants::Any, Wants::GreaterOrEquals(_)) => todo!(),
            (Wants::Any, Wants::Smaller(_)) => todo!(),
            (Wants::Any, Wants::SmallerOrEquals(_)) => todo!(),
            (Wants::Any, Wants::Any) => todo!(),
            (Wants::Any, Wants::Some) => todo!(),
            (Wants::Any, Wants::None) => todo!(),
            (Wants::Some, Wants::Item(_)) => todo!(),
            (Wants::Some, Wants::NotItem(_)) => todo!(),
            (Wants::Some, Wants::Greater(_)) => todo!(),
            (Wants::Some, Wants::GreaterOrEquals(_)) => todo!(),
            (Wants::Some, Wants::Smaller(_)) => todo!(),
            (Wants::Some, Wants::SmallerOrEquals(_)) => todo!(),
            (Wants::Some, Wants::Any) => todo!(),
            (Wants::Some, Wants::Some) => todo!(),
            (Wants::Some, Wants::None) => todo!(),
            (Wants::None, Wants::Item(_)) => todo!(),
            (Wants::None, Wants::NotItem(_)) => todo!(),
            (Wants::None, Wants::Greater(_)) => todo!(),
            (Wants::None, Wants::GreaterOrEquals(_)) => todo!(),
            (Wants::None, Wants::Smaller(_)) => todo!(),
            (Wants::None, Wants::SmallerOrEquals(_)) => todo!(),
            (Wants::None, Wants::Any) => todo!(),
            (Wants::None, Wants::Some) => todo!(),
            (Wants::None, Wants::None) => todo!(),
        }
    }
}

pub fn build_provides(effects:&Vec<Operation>, wants:&HashMap<usize, Wants>) -> Result<HashMap<usize, Wants>, domain::Error> {
    let mut effects_map = HashMap::new(); //wants.clone();
    let mut stack = Vec::new();
    for op in effects {
        match op {
            Operation::ReadState(idx) => {
                if wants.contains_key(idx) {
                    stack.push(*wants.get(idx).unwrap())
                } else {
                    stack.push(Wants::Depends(Operation::Equals, Some(*idx)))
                }
            },
            Operation::WriteState(idx) => {
                let want = stack.pop().unwrap();
                effects_map.insert(*idx, want);
            },
            Operation::Push(literal) => stack.push(Wants::Item(*literal)),
            Operation::Equals => todo!(),
            Operation::Greater => todo!(),
            Operation::Smaller => todo!(),
            Operation::GreaterOrEquals => todo!(),
            Operation::SmallerOrEquals => todo!(),
            Operation::Not => todo!(),
            Operation::And => todo!(),
            Operation::Or => todo!(),
            Operation::Subtract => todo!(),
            Operation::Add => todo!(),
            Operation::Multiply => todo!(),
            Operation::Divide => todo!(),
            Operation::ReadBlackboard(_) => todo!(),
            Operation::WriteBlackboard(_) => todo!(),
            Operation::PlanTask(_) => todo!(),
            Operation::CallOperator(_, _) => todo!(),
        }
    }
    Ok(effects_map)
}

pub fn build_wants(preconditions:&Vec<Operation>) -> Result<HashMap<usize, Wants>, domain::Error> {
    let mut wants_map = HashMap::new();
    // println!("Building wants on {:?}", preconditions);
    let mut wants = vec![Wants::Item(OperandType::B(true))];
    use Operation::*;
    for op in preconditions.iter().rev() {
        // print!("{:?} ", op);
        match op {
            And => (),
            Equals | Greater => wants.push(Wants::Depends(*op, None)),
            Push(literal) => { 
                let last_want = wants.pop().unwrap();
                let new_want = if let Some(want) = wants.last() {
                    want.intersection(&last_want, *literal)?
                } else {
                    todo!("This was the last want...")
                };
                wants.push(new_want);
            },
            ReadState(idx) => {
                let want = wants.pop().unwrap();
                if let Wants::Depends(op, want_idx) = want {
                    if want_idx.is_none() {
                        wants.push(Wants::Depends(op, Some(*idx)));
                    } else {
                        let inv = Wants::Depends(op, Some(*idx)).inverted();
                        // println!("Setting var#{} wants to {:?}", want_idx.unwrap(), inv);
                        wants_map.insert(want_idx.unwrap(), inv);
                    }
                }
                // println!("Setting var#{} wants to {:?}", idx, want);
                // println!("Want stack: {:?}", wants);
                wants_map.insert(*idx, want);
            }
            // Greater => {

            // }
            _ => todo!("Add more operations.")
        }
    }
    Ok(wants_map)
}


// No optimization: {
//     0: [], 
//     1: [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     2: [1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     3: [0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     4: [1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     5: [1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     6: [0, 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     7: [1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     8: [1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     9: [0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     10: [1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18]
//     11: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18], 
//     12: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18], 
//     13: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18], 
//     14: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18], 
//     15: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18], 
//     16: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18], 
//     17: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18], 
//     18: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17], 
// }
// Inertia optimization: {
//     0: [], 
//     1: wants: {3: Item(I(0)), 4: Greater(I(0))}, provides: {3: Greater(I(0)), 4: Item(I(0))}
//     1: [4, 7, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     2: [5, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     3: [6, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     4: wants: {4: Item(I(0)), 3: Item(I(0)), 6: Greater(I(0))}, provides: {3: Greater(I(0)), 6: Item(I(0))}
//     4: [7, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     5: wants: {7: Greater(I(0)), 5: Item(I(0)), 3: Item(I(0))}, provides: {3: Greater(I(0)), 7: Item(I(0))}
//     5: [8, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     6: [9, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     7: [10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     8: [10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     9: [10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     10: [1, 2, 3, 4, 5, 6, 7, 8, 9], 
//     11: [1, 2, 3, 4, 5, 6, 7, 8, 9], 
//     12: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 
//     13: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 
//     14: [1, 2, 3, 4, 5, 6, 7, 8, 9, 11], 
//     15: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 12], 
//     16: [1, 2, 3, 4, 5, 6, 7, 8, 9, 13], 
//     17: [1, 2, 3, 4, 5, 6, 7, 8, 9, 14], 
//     18: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 15]
// }

//  |      |
//  |  2   |
//  |  3   |