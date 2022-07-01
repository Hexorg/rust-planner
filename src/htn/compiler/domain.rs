use std::collections::HashMap;

use crate::htn::{parser::{expression::{Expr, ExpressionVisitor}, statement::{Stmt, StatementVisitor}}, domain::{Task, TaskBody}};

use super::{*, state_ops::StateOpsCompiler};

pub struct DomainCompiler<'a, 'b> {
    compiler: StateOpsCompiler<'a, 'b>,
    task_mapping:&'a mut HashMap<String, usize>,
    operator_mapping:&'a mut HashMap<String, usize>,
    type_mapping: &'a mut HashMap<&'b str, Vec<&'b str>>,
    state_mapping: &'a mut HashMap<String, usize>,
    has_visited_task:bool,
    currently_building_type:Option<&'b str>,
    tasks: Vec<Task>,
    methods: Vec<Task>,
    operations: Vec<Operation>
}

impl<'a, 'b> DomainCompiler<'a, 'b> {
    pub fn new(blackboard_mapping:&'a mut HashMap<String, usize>, task_mapping:&'a mut HashMap<String, usize>, operator_mapping:&'a mut HashMap<String, usize>, type_mapping: &'a mut HashMap<&'b str, Vec<&'b str>>, state_mapping: &'a mut HashMap<String, usize> ) -> Self {
        Self { 
            compiler: StateOpsCompiler::new(None, blackboard_mapping),
            task_mapping,
            operator_mapping,
            type_mapping,
            state_mapping,
            has_visited_task: false,
            currently_building_type: None,
            tasks: Vec::new(),
            methods: Vec::new(),
            operations: Vec::new(),
            }
    }

    pub fn finish(self) -> Vec<Task> {
        self.tasks
    }
} 

impl<'a, 'b> StatementVisitor<'b, (), Error> for DomainCompiler<'a, 'b> {
    fn visit_task_declaration(&mut self, name:&[Token]) -> Result<(), Error> {
        super::get_varpath_idx(self.compiler.substitution, name, &mut self.state_mapping)?;
        Ok(())
    }

    fn visit_task(&mut self, name:&[Token], preconditions:Option<&Expr>, cost:Option<&Expr>, binding:Option<(&str, &str)>, body:&Stmt<'b>, effects:Option<&Stmt>, planning:Option<&Stmt<'b>>) -> Result<(), Error> {
        if let Some((cls, var)) = binding {
            let sub:Vec<(&str, &str)> = self.type_mapping[cls].iter().map(|vname| (var, *vname)).collect();
            for sub in sub {
                let mut state_compiler = StateOpsCompiler::new(Some(sub), &mut self.state_mapping);
                
                let cnames = format!("{}_for_{}", name.last().unwrap().unwrap_identifier(), sub.1);
                let mut cname = Vec::from(name);
                cname.last_mut().unwrap().t = TokenData::Identifier(cnames.as_str());
                super::get_varpath_idx(None, &cname, &mut self.task_mapping)?;
                let preconditions = if let Some(p) = preconditions { p.accept(&mut state_compiler)? } else { vec![Operation::Push(OperandType::B(true))] };
                let effects = if let Some(e) = effects { e.accept(&mut state_compiler)? } else { Vec::new() };
                let cost = if let Some(cost) = cost { cost.accept(&mut state_compiler)?} else { vec![Operation::Push(OperandType::I(0))]};

                let planning = if let Some(p) = planning { p.accept(&mut self.compiler)? } else { Vec::new() };
                let old = self.has_visited_task;
                self.has_visited_task = true;
                body.accept(self)?;
                self.has_visited_task = old;
                if self.methods.len() > 0 {
                    if self.operations.len() > 0 {
                        return Err(body.to_err("Can not use subtasks and operator calls in the same task."))
                    } else {
                        let body = TaskBody::Composite(std::mem::take(&mut self.methods));
                        self.tasks.push(Task{ preconditions, cost, body, effects, planning});
                    }
                } else {
                    let body = TaskBody::Primitive(std::mem::take(&mut self.operations));
                    if self.has_visited_task {
                        self.methods.push(Task{ preconditions, cost, body, effects, planning})
                    } else {
                        self.tasks.push(Task{ preconditions, cost, body, effects, planning});
                    }
                }
            }
        } else {
            let mut state_compiler = StateOpsCompiler::new(None, &mut self.state_mapping);
            super::get_varpath_idx(None, name, &mut self.task_mapping)?;
            let preconditions = if let Some(p) = preconditions { p.accept(&mut state_compiler)? } else { vec![Operation::Push(OperandType::B(true))] };
            let effects = if let Some(e) = effects { e.accept(&mut state_compiler)? } else { Vec::new() };
            let cost = if let Some(cost) = cost { cost.accept(&mut state_compiler)?} else { vec![Operation::Push(OperandType::I(0))]};

            let planning = if let Some(p) = planning { p.accept(&mut self.compiler)? } else { Vec::new() };
            let old = self.has_visited_task;
            self.has_visited_task = true;
            body.accept(self)?;
            self.has_visited_task = old;
            if self.methods.len() > 0 {
                if self.operations.len() > 0 {
                    return Err(body.to_err("Can not use subtasks and operator calls in the same task."))
                } else {
                    let body = TaskBody::Composite(std::mem::take(&mut self.methods));
                    self.tasks.push(Task{ preconditions, cost, body, effects, planning});
                }
            } else {
                let body = TaskBody::Primitive(std::mem::take(&mut self.operations));
                if self.has_visited_task {
                    self.methods.push(Task{ preconditions, cost, body, effects, planning})
                } else {
                    self.tasks.push(Task{ preconditions, cost, body, effects, planning});
                }
            }
        }
        Ok(())
    }

    fn visit_block(&mut self, block:&[Stmt<'b>]) -> Result<(), Error> {
        for stmt in block {
            stmt.accept(self)?;
        }
        Ok(())
    }

    fn visit_expression(&mut self, expr:&Expr<'b>) -> Result<(), Error> {
        let bytecode = expr.accept(self)?;
        self.operations.extend(bytecode);
        Ok(())
    }

    fn visit_include(&mut self, filepath:&Token) -> Result<(), Error> {
        Err(filepath.to_err("Can not include in this context."))
    }

    fn visit_type(&mut self, class:&Token<'b>, body:&Stmt<'b>) -> Result<(), Error> {
        if self.currently_building_type.is_none() {
            let cls = class.unwrap_identifier();
            self.currently_building_type = Some(cls);
            if !self.type_mapping.contains_key(cls) {
                self.type_mapping.insert(cls, Vec::new());
            }
            body.accept(self)?;
            self.currently_building_type = None;
            Ok(())
        } else {
            Err(class.to_err("Unexpected second type statement inside of a previous type statement."))
        }
    }
}

impl<'a, 'b> ExpressionVisitor<'b, Vec<Operation>, Error> for DomainCompiler<'a, 'b> {
    fn visit_binary_expr(&mut self, token: &Token<'b>, left: &Expr<'b>, right: &Expr<'b>) -> Result<Vec<Operation>, Error> {
        self.compiler.visit_binary_expr(token, left, right)
    }

    fn visit_grouping_expr(&mut self, token: &Token<'b>, group: &Expr<'b>) -> Result<Vec<Operation>, Error> {
        self.compiler.visit_grouping_expr(token, group)
    }

    fn visit_literal_expr(&mut self, token: &Token<'b>) -> Result<Vec<Operation>, Error> {
        self.compiler.visit_literal_expr(token)
    }

    fn visit_variable_expr(&mut self, var_path:&[Token<'b>]) -> Result<Vec<Operation>, Error> {
        if let Some(cls) = self.currently_building_type {
            if var_path.len() != 1 {
                return Err(var_path[0].to_err("Unexpected '.' in type definition"));
            } else {
                self.type_mapping.get_mut(cls).unwrap().push(var_path[0].unwrap_identifier());
            }
        }
        let idx = super::get_varpath_idx(self.compiler.substitution, var_path, &mut self.state_mapping)?;
        Ok(vec![Operation::ReadBlackboard(idx)])
    }

    fn visit_unary_expr(&mut self, token: &Token<'b>, right: &Expr<'b>) -> Result<Vec<Operation>, Error> {
        self.compiler.visit_unary_expr(token, right)
    }

    fn visit_assignment_expr(&mut self, var_path:&[Token<'b>], left:&Expr<'b>) -> Result<Vec<Operation>, Error> {
        let idx = super::get_varpath_idx(self.compiler.substitution, var_path, &mut self.state_mapping)?;
        let mut expr = left.accept(self)?;
        expr.push(Operation::WriteBlackboard(idx));
        Ok(expr)
    }

    fn visit_call_expr(&mut self, target: &Token<'b>, args:&[Expr<'b>]) -> Result<Vec<Operation>, Error> {
        let name = target.unwrap_identifier();
        let mut bytecode = Vec::new();
        for arg in args {
            bytecode.extend(arg.accept(self)?);
        }
        if self.task_mapping.contains_key(name) {
            bytecode.push(Operation::PlanTask(self.task_mapping[name]));
        } else {
            if !self.operator_mapping.contains_key(name) {
                self.operator_mapping.insert(name.to_owned(), self.operator_mapping.len());
            }
            bytecode.push(Operation::CallOperator(self.operator_mapping[name], args.len()))
        }
        Ok(bytecode)
    }

    fn visit_nop_expr(&mut self, token: &Token) -> Result<Vec<Operation>, Error> {
        Err(token.to_err("Can not use pass expression in this context."))
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, hash::Hash, iter::FromIterator};

    use crate::htn::parser::{Parser, statement::Stmt};

    use super::{Operation::*, OperandType::*, DomainCompiler };
    #[test]
    fn test_basic() {
        let code = "task test(t < 5) cost t:\n\tt = op()\nplanning:\n\tpop()\neffects:\n\tt = 2";
        let mut parser = Parser::new(code);
        if let Some(Ok(stmt)) = parser.next() {
            let mut state_mapping = HashMap::new();
            let mut blackboard_mapping = HashMap::new();
            let mut task_mapping = HashMap::new();
            let mut operator_mapping = HashMap::new();
            let mut type_mapping = HashMap::new();
            let mut compiler = DomainCompiler::new(&mut blackboard_mapping, &mut task_mapping, &mut operator_mapping, &mut type_mapping, &mut state_mapping);
            let stmt = stmt.accept(&mut compiler);
            assert!(stmt.is_ok(), "{}", stmt.unwrap_err());
            let domain = compiler.finish();
            // println!("{:?}", compiler.task_mapping);
            assert_eq!(state_mapping, HashMap::from([("t".to_owned(), 0)]));
            assert_eq!(blackboard_mapping, HashMap::from([("t".to_owned(), 0)]));
            assert_eq!(operator_mapping, HashMap::from([("op".to_owned(), 0), ("pop".to_owned(), 1)]));
        } else {
            assert!(false); // Parser failed.
        }
    }

}