use std::collections::HashMap;

use crate::htn::parser::{expression::{Expr, ExpressionVisitor}, statement::{Stmt, StatementVisitor}};

use super::*;

pub struct StateOpsCompiler<'a, 'b> {
    pub substitution: Option<(&'b str, &'b str)>,
    pub state_mapping:&'a mut HashMap<String, usize>,
    // pub blackboard_mapping:HashMap<String, usize>,
    // pub operator_mapping:HashMap<String, usize>,
    // task_mapping: HashMap<String, usize>,
}

impl<'a, 'b> StateOpsCompiler<'a, 'b> {
    pub fn new(substitution:Option<(&'b str, &'b str)>, state_mapping:&'a mut HashMap<String, usize>) -> Self {
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

}

impl<'a, 'b> StatementVisitor<'a, Vec<Operation>, Error> for StateOpsCompiler<'a, 'b> {
    fn visit_task_declaration(&mut self, name:&[Token]) -> Result<Vec<Operation>, Error> {
        Err(name[0].to_err("Can not use task statement in this context."))
    }

    fn visit_task(&mut self, name:&[Token], _preconditions:Option<&Expr>, _cost:Option<&Expr>, _binding:Option<(&str, &str)>, _body:&Stmt, _effects:Option<&Stmt>, _planning:Option<&Stmt>) -> Result<Vec<Operation>, Error> {
        Err(name[0].to_err("Can not use task statement in this context."))
    }

    fn visit_block(&mut self, block:&[Stmt<'a>]) -> Result<Vec<Operation>, Error> {
        let mut result = Vec::new();
        for stmt in block {
            result.extend(stmt.accept(self)?);
        }
        Ok(result)
    }

    fn visit_expression(&mut self, expr:&Expr<'a>) -> Result<Vec<Operation>, Error> {
        expr.accept(self)
    }

    fn visit_include(&mut self, filepath:&Token) -> Result<Vec<Operation>, Error> {
        Err(filepath.to_err("Can not use include statement in this context."))
    }

    fn visit_type(&mut self, class:&Token, _body:&Stmt) -> Result<Vec<Operation>, Error> {
        Err(class.to_err("Can not use type statement in this context."))
    }
}

impl<'a, 'b> ExpressionVisitor<'a, Vec<Operation>, Error> for StateOpsCompiler<'a, 'b> {
    fn visit_binary_expr(&mut self, token: &Token<'a>, left: &Expr<'a>, right: &Expr<'a>) -> Result<Vec<Operation>, Error> {
        use TokenData::*;
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
            _ => Err(token.to_err("Unsupported binary operation.").into())
        }
    }

    fn visit_grouping_expr(&mut self, _: &Token, group: &Expr<'a>) -> Result<Vec<Operation>, Error> {
        group.accept(self)
    }

    fn visit_literal_expr(&mut self, token: &Token) -> Result<Vec<Operation>, Error> {
        use std::convert::TryFrom;
        Ok(vec![Operation::Push(OperandType::try_from(*token)?)])
    }

    fn visit_variable_expr(&mut self, var_path:&[Token]) -> Result<Vec<Operation>, Error> {
        let idx = super::get_varpath_idx(self.substitution, var_path, &mut self.state_mapping)?;
        Ok(vec![Operation::ReadState(idx)])
    }

    fn visit_unary_expr(&mut self, token: &Token, right: &Expr<'a>) -> Result<Vec<Operation>, Error> {
        use TokenData::*;
        match token.t {
            Not => {let mut bytecode = right.accept(self)?; bytecode.push(Operation::Not); Ok(bytecode)}
            _ => Err(token.to_err("Unsupported unary operation.").into())
        }
    }

    fn visit_assignment_expr(&mut self, var_path:&[Token], left:&Expr<'a>) -> Result<Vec<Operation>, Error> {
        let idx = super::get_varpath_idx(self.substitution, var_path, &mut self.state_mapping)?;
        let mut expr = left.accept(self)?;
        expr.push(Operation::WriteState(idx));
        Ok(expr)
    }

    fn visit_call_expr(&mut self, target: &Token, _args:&[Expr]) -> Result<Vec<Operation>, Error> {
        Err(target.to_err("Can not call in this context."))
    }

    fn visit_nop_expr(&mut self, token: &Token) -> Result<Vec<Operation>, Error> {
        Err(token.to_err("Can not use pass expression in this context."))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::htn::parser::{Parser, statement::Stmt};

    use super::{Operation::*, OperandType::*, StateOpsCompiler };
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