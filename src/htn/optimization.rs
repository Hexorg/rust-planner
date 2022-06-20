use std::collections::HashMap;

use super::{parser::{expression::ExpressionVisitor, tokens::{Token, TokenData}}, domain::Operation};
use super::domain::{self, OperandType};

#[derive(Debug, Clone, Copy)]
pub enum Inertia {
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

impl Inertia {
    fn bool_eval(&self, op:&Operation, arg:OperandType) -> Result<Inertia, domain::Error> {
        let inertia = match self {
            Self::Item(OperandType::B(v)) => *v,
            Self::NotItem(OperandType::B(v)) => !*v,
            Self::Any => true,
            _ => return Ok(Inertia::None) // (? == 5) can never result in, e.g. 124
        };
        // println!("bool_eval({:?}, {:?} has {} inertia.", op, arg, inertia);
        use Operation::*;
        Ok(match (inertia, op) {
            (true, Equals) => Inertia::Item(arg),
            (true, Smaller) => Inertia::Smaller(arg),
            (true, SmallerOrEquals) => Inertia::SmallerOrEquals(arg),
            (true, Greater) => Inertia::Greater(arg),
            (true, GreaterOrEquals) => Inertia::GreaterOrEquals(arg),
            (true, And) => if arg.is_true() { Inertia::Any} else {Inertia::None},

            
            (false, Equals) => Inertia::NotItem(arg),
            (false, Smaller) => Inertia::GreaterOrEquals(arg),
            (false, SmallerOrEquals) => Inertia::Greater(arg),
            (false, Greater) => Inertia::SmallerOrEquals(arg) ,
            (false, GreaterOrEquals) => Inertia::Smaller(arg),
            (false, And) => if arg.is_true() { Inertia::None} else {Inertia::Any},
            (_, Not) => Inertia::Item(OperandType::B(!inertia)),
            (_, Or) => if arg.is_true() { Inertia::Any} else {Inertia::Item(OperandType::B(inertia))},
            _ => todo!("More operations...?")
            // _ => return Err(domain::Error::Domain(String::new(), "Unexpected operation."))
        })
    }

    pub fn inverted(&self) -> Inertia {
        match self {
            Inertia::Item(i) => Inertia::NotItem(*i),
            Inertia::NotItem(i) => Inertia::Item(*i),
            Inertia::Greater(i) => Inertia::SmallerOrEquals(*i),
            Inertia::GreaterOrEquals(i) => Inertia::Smaller(*i),
            Inertia::Smaller(i) => Inertia::GreaterOrEquals(*i),
            Inertia::SmallerOrEquals(i) => Inertia::Greater(*i),
            Inertia::Depends(op, var) => match op {
                Operation::Equals => Inertia::Depends(Operation::Not, *var),
                Operation::Greater => Inertia::Depends(Operation::SmallerOrEquals, *var),
                Operation::Smaller => Inertia::Depends(Operation::GreaterOrEquals, *var),
                Operation::GreaterOrEquals => Inertia::Depends(Operation::Smaller, *var),
                Operation::SmallerOrEquals => Inertia::Depends(Operation::Greater, *var),
                Operation::Not => Inertia::Depends(Operation::Equals, *var),
                Operation::And => todo!(),
                Operation::Or => todo!(),
                Operation::Subtract => todo!(),
                Operation::Add => todo!(),
                Operation::Multiply => todo!(),
                Operation::Divide => todo!(),
                _ => panic!("Unsupported operation.")
            }
            Inertia::Any => Inertia::None,
            Inertia::Some => Inertia::Some,
            Inertia::None => Inertia::Any,
        }
    }

    pub fn intersection(&self, other:&Self, arg:OperandType) -> Result<Inertia, domain::Error> {
        // println!("Getting intersection of {:?} and {:?} Inertia with arg {:?}.", self, other, arg);
        match (self, other) {
            (Inertia::Item(_), Inertia::Item(_)) => todo!(),
            (Inertia::Item(_), Inertia::NotItem(_)) => todo!(),
            (Inertia::Item(_), Inertia::Greater(_)) => todo!(),
            (Inertia::Item(_), Inertia::GreaterOrEquals(_)) => todo!(),
            (Inertia::Item(_), Inertia::Smaller(_)) => todo!(),
            (Inertia::Item(_), Inertia::SmallerOrEquals(_)) => todo!(),
            (Inertia::Item(_), Inertia::Depends(op, None)) => self.bool_eval(op, arg),
            (Inertia::Item(_), Inertia::Any) => todo!(),
            (Inertia::Item(_), Inertia::Some) => todo!(),
            (Inertia::Item(_), Inertia::None) => todo!(),
            (Inertia::NotItem(_), Inertia::Item(_)) => todo!(),
            (Inertia::NotItem(_), Inertia::NotItem(_)) => todo!(),
            (Inertia::NotItem(_), Inertia::Greater(_)) => todo!(),
            (Inertia::NotItem(_), Inertia::GreaterOrEquals(_)) => todo!(),
            (Inertia::NotItem(_), Inertia::Smaller(_)) => todo!(),
            (Inertia::NotItem(_), Inertia::SmallerOrEquals(_)) => todo!(),
            (Inertia::NotItem(_), Inertia::Depends(_, _)) => todo!(),
            (Inertia::NotItem(_), Inertia::Any) => todo!(),
            (Inertia::NotItem(_), Inertia::Some) => todo!(),
            (Inertia::NotItem(_), Inertia::None) => todo!(),
            (Inertia::Greater(_), Inertia::Item(_)) => todo!(),
            (Inertia::Greater(_), Inertia::NotItem(_)) => todo!(),
            (Inertia::Greater(_), Inertia::Greater(_)) => todo!(),
            (Inertia::Greater(_), Inertia::GreaterOrEquals(_)) => todo!(),
            (Inertia::Greater(_), Inertia::Smaller(_)) => todo!(),
            (Inertia::Greater(_), Inertia::SmallerOrEquals(_)) => todo!(),
            (Inertia::Greater(_), Inertia::Depends(_, _)) => todo!(),
            (Inertia::Greater(_), Inertia::Any) => todo!(),
            (Inertia::Greater(_), Inertia::Some) => todo!(),
            (Inertia::Greater(_), Inertia::None) => todo!(),
            (Inertia::GreaterOrEquals(_), Inertia::Item(_)) => todo!(),
            (Inertia::GreaterOrEquals(_), Inertia::NotItem(_)) => todo!(),
            (Inertia::GreaterOrEquals(_), Inertia::Greater(_)) => todo!(),
            (Inertia::GreaterOrEquals(_), Inertia::GreaterOrEquals(_)) => todo!(),
            (Inertia::GreaterOrEquals(_), Inertia::Smaller(_)) => todo!(),
            (Inertia::GreaterOrEquals(_), Inertia::SmallerOrEquals(_)) => todo!(),
            (Inertia::GreaterOrEquals(_), Inertia::Depends(_, _)) => todo!(),
            (Inertia::GreaterOrEquals(_), Inertia::Any) => todo!(),
            (Inertia::GreaterOrEquals(_), Inertia::Some) => todo!(),
            (Inertia::GreaterOrEquals(_), Inertia::None) => todo!(),
            (Inertia::Smaller(_), Inertia::Item(_)) => todo!(),
            (Inertia::Smaller(_), Inertia::NotItem(_)) => todo!(),
            (Inertia::Smaller(_), Inertia::Greater(_)) => todo!(),
            (Inertia::Smaller(_), Inertia::GreaterOrEquals(_)) => todo!(),
            (Inertia::Smaller(_), Inertia::Smaller(_)) => todo!(),
            (Inertia::Smaller(_), Inertia::SmallerOrEquals(_)) => todo!(),
            (Inertia::Smaller(_), Inertia::Depends(_, _)) => todo!(),
            (Inertia::Smaller(_), Inertia::Any) => todo!(),
            (Inertia::Smaller(_), Inertia::Some) => todo!(),
            (Inertia::Smaller(_), Inertia::None) => todo!(),
            (Inertia::SmallerOrEquals(_), Inertia::Item(_)) => todo!(),
            (Inertia::SmallerOrEquals(_), Inertia::NotItem(_)) => todo!(),
            (Inertia::SmallerOrEquals(_), Inertia::Greater(_)) => todo!(),
            (Inertia::SmallerOrEquals(_), Inertia::GreaterOrEquals(_)) => todo!(),
            (Inertia::SmallerOrEquals(_), Inertia::Smaller(_)) => todo!(),
            (Inertia::SmallerOrEquals(_), Inertia::SmallerOrEquals(_)) => todo!(),
            (Inertia::SmallerOrEquals(_), Inertia::Depends(_, _)) => todo!(),
            (Inertia::SmallerOrEquals(_), Inertia::Any) => todo!(),
            (Inertia::SmallerOrEquals(_), Inertia::Some) => todo!(),
            (Inertia::SmallerOrEquals(_), Inertia::None) => todo!(),
            (Inertia::Depends(_, _), Inertia::Item(_)) => todo!(),
            (Inertia::Depends(_, _), Inertia::NotItem(_)) => todo!(),
            (Inertia::Depends(_, _), Inertia::Greater(_)) => todo!(),
            (Inertia::Depends(_, _), Inertia::GreaterOrEquals(_)) => todo!(),
            (Inertia::Depends(_, _), Inertia::Smaller(_)) => todo!(),
            (Inertia::Depends(_, _), Inertia::SmallerOrEquals(_)) => todo!(),
            (Inertia::Depends(_, _), Inertia::Depends(_, _)) => todo!(),
            (Inertia::Depends(_, _), Inertia::Any) => todo!(),
            (Inertia::Depends(_, _), Inertia::Some) => todo!(),
            (Inertia::Depends(_, _), Inertia::None) => todo!(),
            (Inertia::Any, Inertia::Item(_)) => todo!(),
            (Inertia::Any, Inertia::NotItem(_)) => todo!(),
            (Inertia::Any, Inertia::Greater(_)) => todo!(),
            (Inertia::Any, Inertia::GreaterOrEquals(_)) => todo!(),
            (Inertia::Any, Inertia::Smaller(_)) => todo!(),
            (Inertia::Any, Inertia::SmallerOrEquals(_)) => todo!(),
            (Inertia::Any, Inertia::Depends(_, _)) => todo!(),
            (Inertia::Any, Inertia::Any) => todo!(),
            (Inertia::Any, Inertia::Some) => todo!(),
            (Inertia::Any, Inertia::None) => todo!(),
            (Inertia::Some, Inertia::Item(_)) => todo!(),
            (Inertia::Some, Inertia::NotItem(_)) => todo!(),
            (Inertia::Some, Inertia::Greater(_)) => todo!(),
            (Inertia::Some, Inertia::GreaterOrEquals(_)) => todo!(),
            (Inertia::Some, Inertia::Smaller(_)) => todo!(),
            (Inertia::Some, Inertia::SmallerOrEquals(_)) => todo!(),
            (Inertia::Some, Inertia::Depends(_, _)) => todo!(),
            (Inertia::Some, Inertia::Any) => todo!(),
            (Inertia::Some, Inertia::Some) => todo!(),
            (Inertia::Some, Inertia::None) => todo!(),
            (Inertia::None, Inertia::Item(_)) => todo!(),
            (Inertia::None, Inertia::NotItem(_)) => todo!(),
            (Inertia::None, Inertia::Greater(_)) => todo!(),
            (Inertia::None, Inertia::GreaterOrEquals(_)) => todo!(),
            (Inertia::None, Inertia::Smaller(_)) => todo!(),
            (Inertia::None, Inertia::SmallerOrEquals(_)) => todo!(),
            (Inertia::None, Inertia::Depends(_, _)) => todo!(),
            (Inertia::None, Inertia::Any) => todo!(),
            (Inertia::None, Inertia::Some) => todo!(),
            (Inertia::None, Inertia::None) => todo!(),
            _ => Err(domain::Error::Domain(String::new(), format!("Unexpected want combination: {:?} and {:?}.", self, other))),
        }
    }
    pub fn satisfies(&self, provides:&Inertia) -> bool {
        match (self, provides) {
            (Inertia::Item(want), Inertia::Item(have)) => want == have,
            (Inertia::Item(want), Inertia::NotItem(have)) => want != have,
            (Inertia::Item(want), Inertia::Greater(have)) => want > have,
            (Inertia::Item(want), Inertia::GreaterOrEquals(have)) => want >= have,
            (Inertia::Item(want), Inertia::Smaller(have)) => want < have,
            (Inertia::Item(want), Inertia::SmallerOrEquals(have)) => want <= have,
            (_, Inertia::Depends(_, _)) => true,
            (_, Inertia::Any) => true,
            (_, Inertia::Some) => true,
            (_, Inertia::None) => false,
            (Inertia::NotItem(want), Inertia::Item(have)) => todo!(),
            (Inertia::NotItem(want), Inertia::NotItem(have)) => todo!(),
            (Inertia::NotItem(want), Inertia::Greater(have)) => todo!(),
            (Inertia::NotItem(want), Inertia::GreaterOrEquals(have)) => todo!(),
            (Inertia::NotItem(want), Inertia::Smaller(have)) => todo!(),
            (Inertia::NotItem(want), Inertia::SmallerOrEquals(have)) => todo!(),
            (Inertia::Greater(want), Inertia::Item(have)) => have > want,
            (Inertia::Greater(want), Inertia::NotItem(have)) => true,
            (Inertia::Greater(want), Inertia::Greater(have)) => true,
            (Inertia::Greater(want), Inertia::GreaterOrEquals(have)) => true,
            (Inertia::Greater(want), Inertia::Smaller(have)) => have < want,
            (Inertia::Greater(want), Inertia::SmallerOrEquals(have)) => have <= want,
            (Inertia::GreaterOrEquals(_), Inertia::Item(_)) => todo!(),
            (Inertia::GreaterOrEquals(_), Inertia::NotItem(_)) => todo!(),
            (Inertia::GreaterOrEquals(_), Inertia::Greater(_)) => todo!(),
            (Inertia::GreaterOrEquals(_), Inertia::GreaterOrEquals(_)) => todo!(),
            (Inertia::GreaterOrEquals(_), Inertia::Smaller(_)) => todo!(),
            (Inertia::GreaterOrEquals(_), Inertia::SmallerOrEquals(_)) => todo!(),
            (Inertia::Smaller(_), Inertia::Item(_)) => todo!(),
            (Inertia::Smaller(_), Inertia::NotItem(_)) => todo!(),
            (Inertia::Smaller(_), Inertia::Greater(_)) => todo!(),
            (Inertia::Smaller(_), Inertia::GreaterOrEquals(_)) => todo!(),
            (Inertia::Smaller(_), Inertia::Smaller(_)) => todo!(),
            (Inertia::Smaller(_), Inertia::SmallerOrEquals(_)) => todo!(),
            (Inertia::SmallerOrEquals(_), Inertia::Item(_)) => todo!(),
            (Inertia::SmallerOrEquals(_), Inertia::NotItem(_)) => todo!(),
            (Inertia::SmallerOrEquals(_), Inertia::Greater(_)) => todo!(),
            (Inertia::SmallerOrEquals(_), Inertia::GreaterOrEquals(_)) => todo!(),
            (Inertia::SmallerOrEquals(_), Inertia::Smaller(_)) => todo!(),
            (Inertia::SmallerOrEquals(_), Inertia::SmallerOrEquals(_)) => todo!(),
            (Inertia::Depends(_, _), _) => true,
            (Inertia::Any, _) => true,
            (Inertia::Some, _) => true,
            (Inertia::None, _) => false,

        }
    }
}

pub fn build_provides(effects:&Vec<Operation>, wants:&HashMap<usize, Inertia>) -> Result<HashMap<usize, Inertia>, domain::Error> {
    let mut effects_map = HashMap::new(); //Inertia.clone();
    let mut stack = Vec::new();
    for op in effects {
        match op {
            Operation::ReadState(idx) => {
                if wants.contains_key(idx) {
                    stack.push(*wants.get(idx).unwrap())
                } else {
                    stack.push(Inertia::Depends(Operation::Equals, Some(*idx)))
                }
            },
            Operation::WriteState(idx) => {
                let want = stack.pop().unwrap();
                effects_map.insert(*idx, want);
            },
            Operation::Push(literal) => stack.push(Inertia::Item(*literal)),
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

pub fn build_wants(preconditions:&Vec<Operation>) -> Result<HashMap<usize, Inertia>, domain::Error> {
    let mut wants_map = HashMap::new();
    // println!("Building Inertia on {:?}", preconditions);
    let mut Inertia = vec![Inertia::Item(OperandType::B(true))];
    use Operation::*;
    for op in preconditions.iter().rev() {
        // print!("{:?} ", op);
        match op {
            And => (),
            Equals | Greater => Inertia.push(Inertia::Depends(*op, None)),
            Push(literal) => { 
                let last_want = Inertia.pop().unwrap();
                let new_want = if let Some(want) = Inertia.last() {
                    want.intersection(&last_want, *literal)?
                } else {
                    todo!("This was the last want...")
                };
                Inertia.push(new_want);
            },
            ReadState(idx) => {
                let want = Inertia.pop().unwrap();
                if let Inertia::Depends(op, want_idx) = want {
                    if want_idx.is_none() {
                        Inertia.push(Inertia::Depends(op, Some(*idx)));
                    } else {
                        let inv = Inertia::Depends(op, Some(*idx)).inverted();
                        // println!("Setting var#{} Inertia to {:?}", want_idx.unwrap(), inv);
                        wants_map.insert(want_idx.unwrap(), inv);
                    }
                }
                // println!("Setting var#{} Inertia to {:?}", idx, want);
                // println!("Want stack: {:?}", Inertia);
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
//     1: Inertia: {3: Item(I(0)), 4: Greater(I(0))}, provides: {3: Greater(I(0)), 4: Item(I(0))}
//     1: [4, 7, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     2: [5, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     3: [6, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     4: Inertia: {4: Item(I(0)), 3: Item(I(0)), 6: Greater(I(0))}, provides: {3: Greater(I(0)), 6: Item(I(0))}
//     4: [7, 10, 11, 12, 13, 14, 15, 16, 17, 18], 
//     5: Inertia: {7: Greater(I(0)), 5: Item(I(0)), 3: Item(I(0))}, provides: {3: Greater(I(0)), 7: Item(I(0))}
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