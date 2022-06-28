use std::collections::HashMap;

use crate::htn::parser::expression::{Expr, ExpressionVisitor};

use super::*;

struct StateOpsCompiler<'a> {
    // pub bytecode:Vec<Operation>,
    substitution: Option<(&'a str, &'a str)>,
    // pub is_body: bool,
    // pub is_class_definition: Option<String>,
    state_mapping:&'a mut HashMap<String, usize>,
    // pub blackboard_mapping:HashMap<String, usize>,
    // pub operator_mapping:HashMap<String, usize>,
    // task_mapping: HashMap<String, usize>,
}

impl<'a> StateOpsCompiler<'a> {
    pub fn new(substitution:Option<(&'a str, &'a str)>, state_mapping:&'a mut HashMap<String, usize>) -> Self {
        Self { //bytecode: Vec::new(), 
            state_mapping, 
            substitution,
            // blackboard_mapping: HashMap::new(), 
            // is_body:false, 
            // is_class_definition:None,
            // operator_mapping: HashMap::new(), 
            // task_mapping:HashMap::new(), 
            // type_map:HashMap::new(), 
            // binding: None,
            // substitution_id:0,
            }
    }
    #[inline]
    fn get_varpath_state_idx(&mut self, var_path:&[Token]) -> Result<usize, Error> {
        let mut iter = var_path.iter();
        let first = iter.by_ref().take(1).map(|t| t.unwrap_identifier()).fold(String::new(), |acc, item| {
            acc + if let Some((from, to)) = self.substitution {
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
        if self.state_mapping.contains_key(vname.as_str()) {
            Ok(self.state_mapping[vname.as_str()])
        } else {
            let s = self.state_mapping.len();
            self.state_mapping.insert(vname, s);
            Ok(s)
        }
    }
}

impl<'a> ExpressionVisitor<Vec<Operation>, Error> for StateOpsCompiler<'a> {
    fn visit_binary_expr(&mut self, token: &Token, left: &Expr, right: &Expr) -> Result<Vec<Operation>, Error> {
        let mut bytecode = left.accept(self)?;
        bytecode.extend(right.accept(self)?);
        match token.t {
            NotEquals => {bytecode.push(Operation::Not); bytecode.push(Operation::Equals); Ok(bytecode)},
            EqualsEquals => {bytecode.push(Operation::Equals); Ok(bytecode)},
            Smaller => {bytecode.push(Operation::Smaller); Ok(bytecode)},
            SmallerOrEquals => {bytecode.push(Operation::SmallerOrEquals); Ok(bytecode)},
            Greater => {bytecode.push(Operation::Greater); Ok(bytecode)},
            GreaterOrEquals => {bytecode.push(Operation::GreaterOrEquals); Ok(bytecode)},
            And => {bytecode.push(Operation::And); Ok(bytecode)},
            Or => {bytecode.push(Operation::Or); Ok(bytecode)},
            Minus => {bytecode.push(Operation::Subtract); Ok(bytecode)},
            Plus => {bytecode.push(Operation::Add); Ok(bytecode)},
            Slash => {bytecode.push(Operation::Multiply); Ok(bytecode)},
            Star => {bytecode.push(Operation::Divide); Ok(bytecode)},
            _ => Err(token.to_err("Unsupported binary expression token.").into())
        }
    }

    fn visit_grouping_expr(&mut self, _: &Token, group: &Expr) -> Result<Vec<Operation>, Error> {
        group.accept(self)
    }

    fn visit_literal_expr(&mut self, token: &Token) -> Result<Vec<Operation>, Error> {
        use std::convert::TryFrom;
        Ok(vec![Operation::Push(OperandType::try_from(*token)?)])
    }

    fn visit_variable_expr(&mut self, var_path:&[Token]) -> Result<Vec<Operation>, Error> {
        let idx = self.get_varpath_state_idx(var_path)?;
        Ok(vec![Operation::ReadState(idx)])
    }

    fn visit_unary_expr(&mut self, token: &Token, right: &Expr) -> Result<Vec<Operation>, Error> {
        match token.t {
            Not => {let mut bytecode = right.accept(self)?; bytecode.push(Operation::Not); Ok(bytecode)}
            _ => Err(token.to_err("Unsupported unary operation.").into())
        }
    }

    fn visit_assignment_expr(&mut self, var_path:&[Token], left:&Expr) -> Result<Vec<Operation>, Error> {
        let idx = self.get_varpath_state_idx(var_path)?;
        let mut expr = left.accept(self)?;
        expr.push(Operation::WriteState(idx));
        Ok(expr)
    }

    fn visit_call_expr(&mut self, target: &Token, args:&[Expr]) -> Result<Vec<Operation>, Error> {
        panic!("Unable to call in preconditions.")
    }

    fn visit_nop_expr(&mut self, _: &Token) -> Result<Vec<Operation>, Error> {
        panic!("Unable to NOP in preconditions. Use preconditions = None instead.")
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, hash::Hash, iter::FromIterator};

    use crate::htn::parser::{Parser, statement::Stmt};

    use super::{Operation::*, OperandType::*, StateOpsCompiler};
    #[test]
    fn test_basic() {
        let code = "p = v + 5 - 2";
        let mut parser = Parser::new(code);
        if let Some(Ok(Stmt::Expression(expr))) = parser.next() {
            let mut mapping = HashMap::new();
            let mut compiler = StateOpsCompiler::new(None, &mut mapping);
            let bytecode = expr.accept(&mut compiler);
            assert!(bytecode.is_ok());
            let bytecode = bytecode.unwrap();
            assert_eq!(bytecode, vec![ReadState(1), Push(I(5)), Add, Push(I(2)), Subtract, WriteState(0)]);
            assert_eq!(mapping, HashMap::from([("v".to_owned(), 1), ("p".to_owned(), 0)]));
        } else {
            assert!(false); // Parser failed.
        }
    }
    #[test]
    fn test_binding() {
        let code = "p = c + 5 - 2";
        let mut parser = Parser::new(code);
        if let Some(Ok(Stmt::Expression(expr))) = parser.next() {
            let mut mapping = HashMap::new();
            let mut compiler = StateOpsCompiler::new(Some(("c", "v")), &mut mapping);
            let bytecode = expr.accept(&mut compiler);
            assert!(bytecode.is_ok());
            let bytecode = bytecode.unwrap();
            assert_eq!(bytecode, vec![ReadState(1), Push(I(5)), Add, Push(I(2)), Subtract, WriteState(0)]);
            assert_eq!(mapping, HashMap::from([("v".to_owned(), 1), ("p".to_owned(), 0)]));
        } else {
            assert!(false); // Parser failed.
        }
    }
}