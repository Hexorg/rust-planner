use std::collections::HashMap;

use super::parser::{expression::{Expr, ExpressionVisitor}, statement::{Stmt, StatementVisitor}};
use super::{*, state_ops::StateOpsCompiler, optimization::{build_provides, build_wants}};


pub struct DomainCompiler<'a, 'b> {
    compiler: StateOpsCompiler<'a, 'b>,
    task_mapping:&'a mut HashMap<String, usize>,
    operator_mapping:&'a mut HashMap<String, usize>,
    type_mapping: &'a mut HashMap<String, Vec<String>>,
    state_mapping: &'a mut HashMap<String, usize>,
    parent_task:Option<String>,
    currently_building_type:Option<String>,
    pass:usize,
    tasks: Vec<Task>,
    // task_names: Vec<String>,
    methods: Vec<usize>,
    operations: Vec<Operation>
}

impl<'a, 'b> DomainCompiler<'a, 'b> {
    const MAX_PASSES:usize = 2;
    pub fn new(blackboard_mapping:&'a mut HashMap<String, usize>, 
                task_mapping:&'a mut HashMap<String, usize>, 
                operator_mapping:&'a mut HashMap<String, usize>, 
                type_mapping: &'a mut HashMap<String, Vec<String>>, 
                state_mapping: &'a mut HashMap<String, usize>) -> Self {
        Self { 
            compiler: StateOpsCompiler::new(None, blackboard_mapping),
            task_mapping,
            operator_mapping,
            type_mapping,
            state_mapping,
            parent_task: None,
            currently_building_type: None,
            pass:0,
            tasks: Vec::new(),
            // task_names: Vec::new(),
            methods: Vec::new(),
            operations: Vec::new(),
            }
    }

    pub fn finish(self) -> Vec<Task> {
        self.tasks
    }

} 

impl<'a, 'b> StatementVisitor<'b, (), Error> for DomainCompiler<'a, 'b> {
    fn visit_task_declaration(&mut self, name:&[Token], binding:Option<(&'b str, &'b str)>) -> Result<(), Error> {
        let substitutions = if let Some((cls, body_variable)) = binding {
            if !self.type_mapping.contains_key(cls) {
                return Err(name[0].to_err(&format!("Undeclared variable type '{}'.", cls)).into())
            }
            self.type_mapping[cls].iter().map(|class_variable| Some((String::from(body_variable), class_variable.clone()))).collect()
        } else {
            vec![None] // Forces the next for loop to iterate at least once
        };

        for substitution in substitutions {
            let sub = substitution.as_ref().and_then(|(l,r)| Some((l.as_str(), r.as_str())));
            let my_name = if let Some(sub) = sub {
                // replace last identifier
                let last = format!("{}_for_{}", name.last().unwrap().unwrap_identifier(), sub.1);
                let mut cname = Vec::from(name);
                cname.last_mut().unwrap().t = TokenData::Identifier(last.as_str());
                super::varpath_to_string(None, &cname)
            } else {
                super::varpath_to_string(None, name)
            };
            // Fully qualified domain name is my_name for top-level tasks, or parent.my_name otherwise
            let fqdn = if let Some(ref parent) = self.parent_task {
                format!("{}.{}", parent, my_name)
            } else {
                my_name
            };
            let my_id = if self.task_mapping.contains_key(&fqdn) { 
                self.task_mapping[&fqdn]
            } else {
                let r = self.task_mapping.len();
                self.task_mapping.insert(fqdn.clone(), r);
                // At this point we haven't built the body yet.
                // This is needed because child tasks need to know parent's ID but 
                // parent's body can't be built until after children
                self.tasks.push(Task::default());
                r
            };
            if self.pass == 1 && self.parent_task.is_some() {
                self.tasks[my_id].is_method = true;
                self.methods.push(my_id)
            }
        }
        Ok(())
    }

    fn visit_task(&mut self, name:&[Token<'b>], preconditions:Option<&Expr<'b>>, cost:Option<&Expr<'b>>, binding:Option<(&'b str, &'b str)>, body:&Stmt<'b>, effects:Option<&Stmt<'b>>, planning:Option<&Stmt<'b>>) -> Result<(), Error> {
        match self.pass {
            0 => self.visit_task_declaration(name, binding),
            1 => {
                // Step 1: Prepare vector of substitutions. Substitution is on format (from, to), or None if we don't need any
                let substitutions = if let Some((cls, body_variable)) = binding {
                    if !self.type_mapping.contains_key(cls) {
                        return Err(name[0].to_err(&format!("Undeclared variable type '{}'.", cls)).into())
                    }
                    self.type_mapping[cls].iter().map(|class_variable| Some((String::from(body_variable), class_variable.clone()))).collect()
                } else {
                    vec![None] // Forces the next for loop to iterate at least once
                };
                for substitution in substitutions {
                    let sub = substitution.as_ref().and_then(|(l,r)| Some((l.as_str(), r.as_str())));
                    let mut state_compiler = StateOpsCompiler::new(sub, &mut self.state_mapping);
                    // Task name is a chain of identifier.idenfier.identifier...identifier. Convert this to my_name:String
                    let my_name = if let Some(sub) = sub {
                        // replace last identifier
                        let last = format!("{}_for_{}", name.last().unwrap().unwrap_identifier(), sub.1);
                        let mut cname = Vec::from(name);
                        cname.last_mut().unwrap().t = TokenData::Identifier(last.as_str());
                        super::varpath_to_string(None, &cname)
                    } else {
                        super::varpath_to_string(None, name)
                    };
                    // Fully qualified domain name is my_name for top-level tasks, or parent.my_name otherwise
                    let fqdn = if let Some(ref parent) = self.parent_task {
                        format!("{}.{}", parent, my_name)
                    } else {
                        my_name
                    };
                    // Step 2: compile task-related fields
                    let preconditions = if let Some(p) = preconditions { p.accept(&mut state_compiler)? } else { vec![Operation::Push(OperandType::B(true))] };
                    let wants = match build_wants(&preconditions) {
                        Ok(w) => w,
                        Err(e) => {return Err(name[0].to_err(&e.message).into())}
                    };
                    let effects = if let Some(e) = effects { e.accept(&mut state_compiler)? } else { Vec::new() };
                    let provides = match build_provides(&effects, &wants) {
                        Ok(p) => p,
                        Err(e) => {return Err(name[0].to_err(&e.message).into())}
                    };
                    let cost = if let Some(cost) = cost { cost.accept(&mut state_compiler)?} else { vec![Operation::Push(OperandType::I(0))]};

                    let planning = if let Some(p) = planning { p.accept(self)?; std::mem::take(&mut self.operations) } else { Vec::new() };
                    let is_method = self.parent_task.is_some();
                    // Step 3: Figure out id of this task in self.tasks[] vector. 
                    let my_id = if self.task_mapping.contains_key(&fqdn) { 
                        // if my id exists in task mapping, it was likely created from the first pass
                        // or the task declaration statement
                        // TODO detect declared but not defined tasks
                        let my_id = self.task_mapping[&fqdn];
                        self.tasks[my_id].preconditions = preconditions;
                        self.tasks[my_id].cost = cost;
                        self.tasks[my_id].wants = wants;
                        self.tasks[my_id].effects = effects;
                        self.tasks[my_id].provides = provides;
                        self.tasks[my_id].planning = planning;
                        self.tasks[my_id].is_method |= is_method; // OR'ing because task declaration my have already specified this task to be a method
                        my_id
                    } else {
                        let r = self.task_mapping.len();
                        self.task_mapping.insert(fqdn.clone(), r);
                        // At this point we haven't built the body yet.
                        // This is needed because child tasks need to know parent's ID but 
                        // parent's body can't be built until after children
                        self.tasks.push(Task{ preconditions, cost, body:TaskBody::Primitive(Vec::new()), effects, planning, wants, provides, is_method});
                        r
                    };
                    let my_parent_task = std::mem::take(&mut self.parent_task);
                    self.parent_task = Some(fqdn);
                    body.accept(self)?;
                    self.parent_task = my_parent_task;
                    // Step 4: Copy the built body to already allocated self.tasks[my_id]
                    if self.operations.len() > 0 {
                        self.tasks[my_id].body = TaskBody::Primitive(std::mem::take(&mut self.operations));
                        if self.parent_task.is_some() {
                            self.methods.push(my_id)
                        }
                    } else {
                        self.tasks[my_id].body = TaskBody::Composite(std::mem::take(&mut self.methods));
                    }
                }
                Ok(())
            },
            _ => Ok(())
        }
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
        if self.pass == 0 {
            if let Token{t:TokenData::Literal(Literal::S(filepath)),..} = filepath {
                match std::fs::read_to_string(filepath) {
                    Ok(code) => {
                        let mut errors = Vec::new();
                        let mut inc_compiler = DomainCompiler::new(self.compiler.state_mapping, self.task_mapping, self.operator_mapping, self.type_mapping, self.state_mapping);
                        while inc_compiler.pass < DomainCompiler::MAX_PASSES {
                            for result in parser::Parser::new(code.as_str()) {
                                match result {
                                    Ok(stmt) =>  {
                                        match stmt.accept(&mut inc_compiler) {
                                            Ok(()) => (),
                                            Err(e) => errors.push(e),
                                        };
                                    },
                                    Err(e) => errors.push(Error::Parser(Some(String::from(*filepath)), e)),
                                }
                            }
                            if errors.len() > 0 {
                                break;
                            }
                            inc_compiler.pass += 1;
                        }
                        if errors.len() > 0 {
                            Err(Error::FromFile(String::from(*filepath), errors))
                        } else {
                            // let tn = std::mem::take(&mut self.task_names);
                            // inc_compiler.task_names.extend(tn);
                            let t = std::mem::take(&mut self.tasks);
                            inc_compiler.tasks.extend(t);
                            // self.task_names = inc_compiler.task_names;
                            self.tasks = inc_compiler.tasks;
                            Ok(())
                        }
                    },
                    Err(e) => Err(Error::Basic(String::from(*filepath), e.to_string())),
                }
            } else {
                Err(filepath.to_err("Expected a string literal.").into())
            }
        } else {
            Ok(())
        }
    }

    fn visit_type(&mut self, class:&Token<'b>, body:&Stmt<'b>) -> Result<(), Error> {
        if self.pass == 0 {
            if self.currently_building_type.is_none() {
                let cls = class.unwrap_identifier();
                self.currently_building_type = Some(String::from(cls));
                if !self.type_mapping.contains_key(cls) {
                    self.type_mapping.insert(String::from(cls), Vec::new());
                }
                body.accept(self)?;
                self.currently_building_type = None;
                Ok(())
            } else {
                Err(class.to_err("Unexpected second type statement inside of a previous type statement.").into())
            }
        } else {
            Ok(())
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
        if let Some(ref cls) = self.currently_building_type {
            if var_path.len() != 1 {
                Err(var_path[0].to_err("Unexpected '.' in type definition").into())
            } else {
                self.type_mapping.get_mut(cls).unwrap().push(String::from(var_path[0].unwrap_identifier()));
                Ok(Vec::new())
            }
        } else {
            let idx = super::get_varpath_idx(self.compiler.substitution, var_path, &mut self.compiler.state_mapping);
            Ok(vec![Operation::ReadBlackboard(idx)])
        }
    }

    fn visit_unary_expr(&mut self, token: &Token<'b>, right: &Expr<'b>) -> Result<Vec<Operation>, Error> {
        self.compiler.visit_unary_expr(token, right)
    }

    fn visit_assignment_expr(&mut self, var_path:&[Token<'b>], left:&Expr<'b>) -> Result<Vec<Operation>, Error> {
        let idx = super::get_varpath_idx(self.compiler.substitution, var_path, &mut self.compiler.state_mapping);
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
        Err(token.to_err("Can not use pass expression in this context.").into())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::htn::parser::Parser;

    use super::{Operation::*, OperandType::*, DomainCompiler, Task, TaskBody};
    use super::super::optimization::Inertia;
    #[test]
    fn test_basic() {
        let code = "task test(t < 5) cost t:\n\tt = op()\nplanning:\n\tpop(r)\neffects:\n\tt = 2";
        let mut state_mapping = HashMap::new();
        let mut blackboard_mapping = HashMap::new();
        let mut task_mapping = HashMap::new();
        let mut operator_mapping = HashMap::new();
        let mut type_mapping = HashMap::new();
        let mut compiler = DomainCompiler::new(&mut blackboard_mapping, &mut task_mapping, &mut operator_mapping, &mut type_mapping, &mut state_mapping);
        while compiler.pass < DomainCompiler::MAX_PASSES {
            for stmt in Parser::new(code) {
                stmt.expect("Unexpected parsing error").accept(&mut compiler).expect("Unexpected compilation error in unit-tests");
            }
            compiler.pass += 1;
        }
            
        let tasks = compiler.finish();
        assert_eq!(task_mapping, HashMap::from([("test".to_owned(), 0)]));
        assert_eq!(state_mapping, HashMap::from([("t".to_owned(), 0)]));
        assert_eq!(blackboard_mapping, HashMap::from([("r".to_owned(), 0), ("t".to_owned(), 1)]));
        assert_eq!(operator_mapping, HashMap::from([("pop".to_owned(), 0), ("op".to_owned(), 1)]));
        assert_eq!(tasks, vec![Task{ 
            preconditions: vec![ReadState(0),Push(I(5)),Smaller], 
            cost: vec![ReadState(0)], 
            body: TaskBody::Primitive(vec![CallOperator(1, 0), WriteBlackboard(1)]), 
            effects: vec![Push(I(2)), WriteState(0)], 
            planning: vec![ReadBlackboard(0), CallOperator(0, 1)],
            is_method: false,
            wants: HashMap::from([(0, Inertia::Smaller(I(5)))]),
            provides: HashMap::from([(0, Inertia::Item(I(2)))]),
        }])
    }

    #[test]
    fn test_composite() {
        let code = "task t1:\n\ttask m1:\n\t\top1()\n\ttask m2:\n\t\top2()";
        let mut state_mapping = HashMap::new();
        let mut blackboard_mapping = HashMap::new();
        let mut task_mapping = HashMap::new();
        let mut operator_mapping = HashMap::new();
        let mut type_mapping = HashMap::new();
        let mut compiler = DomainCompiler::new(&mut blackboard_mapping, &mut task_mapping, &mut operator_mapping, &mut type_mapping, &mut state_mapping);
        while compiler.pass < DomainCompiler::MAX_PASSES {
            for stmt in Parser::new(code) {
                stmt.expect("Unexpected parsing error").accept(&mut compiler).expect("Unexpected compilation error in unit-tests");
            }
            compiler.pass += 1;
        }
            
        let tasks = compiler.finish();
        assert_eq!(task_mapping, HashMap::from([("t1.m1".to_owned(), 1), ("t1.m2".to_owned(), 2), ("t1".to_owned(), 0)]));
        assert_eq!(state_mapping, HashMap::new());
        assert_eq!(blackboard_mapping, HashMap::new());
        assert_eq!(operator_mapping, HashMap::from([("op1".to_owned(), 0), ("op2".to_owned(), 1)]));
        assert_eq!(tasks, vec![
            Task{
                preconditions: vec![Push(B(true))], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Composite(vec![1, 2]), 
                effects: vec![], 
                planning: vec![],
                is_method: false,
                wants: HashMap::new(),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![Push(B(true))],
                cost: vec![Push(I(0))], 
                effects: vec![], 
                planning: vec![],
                wants: HashMap::new(),
                provides: HashMap::new(),
                body: TaskBody::Primitive(vec![CallOperator(0, 0)]),
                is_method: true,
            },
            Task{
                preconditions: vec![Push(B(true))],
                cost: vec![Push(I(0))], 
                effects: vec![], 
                planning: vec![],
                wants: HashMap::new(),
                provides: HashMap::new(),
                body: TaskBody::Primitive(vec![CallOperator(1, 0)]),
                is_method: true,
            }])
    }

    #[test]
    fn test_include() {
        use tempfile::Builder;
        use std::io::Write;
        let mut file = Builder::new().suffix(".htn").tempfile().expect("Unable to create temporary file");
        writeln!(file.as_file_mut(), "task t1:\n\ttask m1:\n\t\top1()\n\ttask m2:\n\t\top2()").expect("Unable to write to tempfile");
        let code = format!("include \"{}\"\ntask Main:\n\tt1()", file.path().display());
        let mut state_mapping = HashMap::new();
        let mut blackboard_mapping = HashMap::new();
        let mut task_mapping = HashMap::new();
        let mut operator_mapping = HashMap::new();
        let mut type_mapping = HashMap::new();
        let mut compiler = DomainCompiler::new(&mut blackboard_mapping, &mut task_mapping, &mut operator_mapping, &mut type_mapping, &mut state_mapping);
        while compiler.pass < DomainCompiler::MAX_PASSES {
            for stmt in Parser::new(code.as_str()) {
                stmt.expect("Unexpected parsing error").accept(&mut compiler).expect("Unexpected compilation error in unit-tests");
            }
            compiler.pass += 1;
        }
        let tasks = compiler.finish();
        assert_eq!(task_mapping, HashMap::from([("t1.m1".to_owned(), 1), ("t1.m2".to_owned(), 2), ("t1".to_owned(), 0), ("Main".to_owned(), 3)]));
        assert_eq!(state_mapping, HashMap::new());
        assert_eq!(blackboard_mapping, HashMap::new());
        assert_eq!(operator_mapping, HashMap::from([("op1".to_owned(), 0), ("op2".to_owned(), 1)]));
        assert_eq!(tasks, vec![
            Task{ // t1
                preconditions: vec![Push(B(true))], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Composite(vec![1, 2]), 
                effects: vec![], 
                planning: vec![],
                is_method: false,
                wants: HashMap::new(),
                provides: HashMap::new(),
            },
            Task{ // t1.m1
                preconditions: vec![Push(B(true))],
                cost: vec![Push(I(0))], 
                effects: vec![], 
                planning: vec![],
                wants: HashMap::new(),
                provides: HashMap::new(),
                body: TaskBody::Primitive(vec![CallOperator(0, 0)]),
                is_method: true,
            },
            Task{ // t1.m2
                preconditions: vec![Push(B(true))],
                cost: vec![Push(I(0))], 
                effects: vec![], 
                planning: vec![],
                wants: HashMap::new(),
                provides: HashMap::new(),
                body: TaskBody::Primitive(vec![CallOperator(1, 0)]),
                is_method: true,
            },
            Task{ // Main
                preconditions: vec![Push(B(true))], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![PlanTask(0)]), 
                effects: vec![], 
                planning: vec![],
                is_method: false,
                wants: HashMap::new(),
                provides: HashMap::new(),
            }])
    }

    #[test]
    fn test_task_declaration() {
        let code = "task t1:\n\ttask t2\n\ntask t1.t2:\n\top()";
        let mut state_mapping = HashMap::new();
        let mut blackboard_mapping = HashMap::new();
        let mut task_mapping = HashMap::new();
        let mut operator_mapping = HashMap::new();
        let mut type_mapping = HashMap::new();
        let mut compiler = DomainCompiler::new(&mut blackboard_mapping, &mut task_mapping, &mut operator_mapping, &mut type_mapping, &mut state_mapping);
        while compiler.pass < DomainCompiler::MAX_PASSES {
            for stmt in Parser::new(code) {
                stmt.expect("Unexpected parsing error").accept(&mut compiler).expect("Unexpected compilation error in unit-tests");
            }
            compiler.pass += 1;
        }

        let tasks = compiler.finish();
        assert_eq!(task_mapping, HashMap::from([("t1".to_owned(), 0), ("t1.t2".to_owned(), 1)]));
        assert_eq!(state_mapping, HashMap::new());
        assert_eq!(blackboard_mapping, HashMap::new());
        assert_eq!(operator_mapping, HashMap::from([("op".to_owned(), 0)]));
        assert_eq!(tasks, vec![
            Task{
                preconditions: vec![Push(B(true))], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Composite(vec![1]), 
                effects: vec![], 
                planning: vec![],
                is_method: false,
                wants: HashMap::new(),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![Push(B(true))], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(0, 0)]), 
                effects: vec![], 
                planning: vec![],
                is_method: true,
                wants: HashMap::new(),
                provides: HashMap::new(),
            },
        ]);
    }

    #[test]
    fn test_forward_declaration() {
        let code = "task t1:\n\tt2()\ntask t2:\n\top()";
        let mut state_mapping = HashMap::new();
        let mut blackboard_mapping = HashMap::new();
        let mut task_mapping = HashMap::new();
        let mut operator_mapping = HashMap::new();
        let mut type_mapping = HashMap::new();
        let mut compiler = DomainCompiler::new(&mut blackboard_mapping, &mut task_mapping, &mut operator_mapping, &mut type_mapping, &mut state_mapping);
        while compiler.pass < DomainCompiler::MAX_PASSES {
            for stmt in Parser::new(code) {
                stmt.expect("Unexpected parsing error").accept(&mut compiler).expect("Unexpected compilation error in unit-tests");
            }
            compiler.pass += 1;
        }

        let tasks = compiler.finish();
        assert_eq!(task_mapping, HashMap::from([("t1".to_owned(), 0), ("t2".to_owned(), 1)]));
        assert_eq!(state_mapping, HashMap::new());
        assert_eq!(blackboard_mapping, HashMap::new());
        assert_eq!(operator_mapping, HashMap::from([("op".to_owned(), 0)]));
        assert_eq!(tasks, vec![
            Task{
                preconditions: vec![Push(B(true))], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![PlanTask(1)]), 
                effects: vec![], 
                planning: vec![],
                is_method: false,
                wants: HashMap::new(),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![Push(B(true))], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(0, 0)]), 
                effects: vec![], 
                planning: vec![],
                is_method: false,
                wants: HashMap::new(),
                provides: HashMap::new(),
            },
        ]);
    }

    #[test]
    fn test_hashmap_typing() {
        let code = "task t1:\n\ttask s1(cell.is_empty) for Cell as cell:\n\t\tops1()\n\ttask s2(block.is_empty) for Block as block:\n\t\tops2()";
        let mut state_mapping = HashMap::new();
        let mut blackboard_mapping = HashMap::new();
        let mut task_mapping = HashMap::new();
        let mut operator_mapping = HashMap::new();
        let mut type_mapping = HashMap::from([("Cell".to_owned(), vec!["a1".to_owned(), "a2".to_owned()]), ("Block".to_owned(), vec!["b1".to_owned(), "b2".to_owned()])]);
        let mut compiler = DomainCompiler::new(&mut blackboard_mapping, &mut task_mapping, &mut operator_mapping, &mut type_mapping, &mut state_mapping);
        while compiler.pass < DomainCompiler::MAX_PASSES {
            for stmt in Parser::new(code) {
                stmt.expect("Unexpected parsing error").accept(&mut compiler).expect("Unexpected compilation error in unit-tests");
            }
            compiler.pass += 1;
        }

        let tasks = compiler.finish();
        assert_eq!(task_mapping, HashMap::from([("t1".to_owned(), 0), ("t1.s1_for_a1".to_owned(), 1), ("t1.s1_for_a2".to_owned(), 2), ("t1.s2_for_b1".to_owned(), 3), ("t1.s2_for_b2".to_owned(), 4)]));
        assert_eq!(state_mapping, HashMap::from([("a1.is_empty".to_owned(), 0), ("a2.is_empty".to_owned(), 1), ("b1.is_empty".to_owned(), 2), ("b2.is_empty".to_owned(), 3)]));
        assert_eq!(blackboard_mapping, HashMap::new());
        assert_eq!(operator_mapping, HashMap::from([("ops1".to_owned(), 0), ("ops2".to_owned(), 1)]));
        assert_eq!(tasks, vec![
            Task{
                preconditions: vec![Push(B(true))], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Composite(vec![1, 2, 3, 4]), 
                effects: vec![], 
                planning: vec![],
                is_method: false,
                wants: HashMap::new(),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(0)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(0, 0)]), 
                effects: vec![], 
                planning: vec![],
                is_method: true,
                wants: HashMap::from([(0, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(1)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(0, 0)]), 
                effects: vec![], 
                planning: vec![],
                is_method: true,
                wants: HashMap::from([(1, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(2)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(1, 0)]), 
                effects: vec![], 
                planning: vec![],
                is_method: true,
                wants: HashMap::from([(2, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(3)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(1, 0)]), 
                effects: vec![], 
                planning: vec![],
                is_method: true,
                wants: HashMap::from([(3, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            },
        ]);
    }

    #[test]
    fn test_type_statement() {
        let code = "type Cell:\n\ta1\n\ta2\n";
        let mut state_mapping = HashMap::new();
        let mut blackboard_mapping = HashMap::new();
        let mut task_mapping = HashMap::new();
        let mut operator_mapping = HashMap::new();
        let mut type_mapping = HashMap::new();
        let mut compiler = DomainCompiler::new(&mut blackboard_mapping, &mut task_mapping, &mut operator_mapping, &mut type_mapping, &mut state_mapping);
        while compiler.pass < DomainCompiler::MAX_PASSES {
            for stmt in Parser::new(code) {
                stmt.expect("Unexpected parsing error").accept(&mut compiler).expect("Unexpected compilation error in unit-tests");
            }
            compiler.pass += 1;
        }

        let tasks = compiler.finish();
        assert_eq!(type_mapping, HashMap::from([("Cell".to_owned(), vec!["a1".to_owned(), "a2".to_owned()])]));
        assert_eq!(task_mapping, HashMap::new());
        assert_eq!(state_mapping, HashMap::new());
        assert_eq!(blackboard_mapping, HashMap::new());
        assert_eq!(operator_mapping, HashMap::new());
        assert_eq!(tasks, Vec::new());
    }

    #[test]
    fn test_source_typing() {
        let code = "type Cell:\n\ta1\n\ta2\ntype Block:\n\tb1\n\tb2\ntask t1:\n\ttask s1(cell.is_empty) for Cell as cell:\n\t\tops1()\n\ttask s2(block.is_empty) for Block as block:\n\t\tops2()";
        let mut state_mapping = HashMap::new();
        let mut blackboard_mapping = HashMap::new();
        let mut task_mapping = HashMap::new();
        let mut operator_mapping = HashMap::new();
        let mut type_mapping = HashMap::new();
        let mut compiler = DomainCompiler::new(&mut blackboard_mapping, &mut task_mapping, &mut operator_mapping, &mut type_mapping, &mut state_mapping);
        while compiler.pass < DomainCompiler::MAX_PASSES {
            for stmt in Parser::new(code) {
                stmt.expect("Unexpected parsing error").accept(&mut compiler).expect("Unexpected compilation error in unit-tests");
            }
            compiler.pass += 1;
        }

        let tasks = compiler.finish();
        assert_eq!(type_mapping, HashMap::from([("Block".to_owned(), vec!["b1".to_owned(), "b2".to_owned()]), ("Cell".to_owned(), vec!["a1".to_owned(), "a2".to_owned()])]));
        assert_eq!(task_mapping, HashMap::from([("t1".to_owned(), 0), ("t1.s1_for_a1".to_owned(), 1), ("t1.s1_for_a2".to_owned(), 2), ("t1.s2_for_b1".to_owned(), 3), ("t1.s2_for_b2".to_owned(), 4)]));
        assert_eq!(state_mapping, HashMap::from([("a1.is_empty".to_owned(), 0), ("a2.is_empty".to_owned(), 1), ("b1.is_empty".to_owned(), 2), ("b2.is_empty".to_owned(), 3)]));
        assert_eq!(blackboard_mapping, HashMap::new());
        assert_eq!(operator_mapping, HashMap::from([("ops1".to_owned(), 0), ("ops2".to_owned(), 1)]));
        assert_eq!(tasks, vec![
            Task{
                preconditions: vec![Push(B(true))], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Composite(vec![1, 2, 3, 4]), 
                effects: vec![], 
                planning: vec![],
                is_method: false,
                wants: HashMap::new(),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(0)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(0, 0)]), 
                effects: vec![], 
                planning: vec![],
                is_method: true,
                wants: HashMap::from([(0, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(1)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(0, 0)]), 
                effects: vec![], 
                planning: vec![],
                is_method: true,
                wants: HashMap::from([(1, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(2)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(1, 0)]), 
                effects: vec![], 
                planning: vec![],
                is_method: true,
                wants: HashMap::from([(2, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(3)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(1, 0)]), 
                effects: vec![], 
                planning: vec![],
                is_method: true,
                wants: HashMap::from([(3, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            },
        ]);
    }

    #[test]
    fn test_single_typing() {
        let code = "task t1:\n\ttask s1(cell.is_empty) for Cell as cell:\n\t\tops1()\n\ttask s2(block.is_empty) for Block as block:\n\t\tops2()";
        let mut state_mapping = HashMap::new();
        let mut blackboard_mapping = HashMap::new();
        let mut task_mapping = HashMap::new();
        let mut operator_mapping = HashMap::new();
        let mut type_mapping = HashMap::from([("Cell".to_owned(), vec!["a1".to_owned()]), ("Block".to_owned(), vec!["b1".to_owned()])]);
        let mut compiler = DomainCompiler::new(&mut blackboard_mapping, &mut task_mapping, &mut operator_mapping, &mut type_mapping, &mut state_mapping);
        while compiler.pass < DomainCompiler::MAX_PASSES {
            for stmt in Parser::new(code) {
                stmt.expect("Unexpected parsing error").accept(&mut compiler).expect("Unexpected compilation error in unit-tests");
            }
            compiler.pass += 1;
        }

        let tasks = compiler.finish();
        assert_eq!(task_mapping, HashMap::from([("t1".to_owned(), 0), ("t1.s1_for_a1".to_owned(), 1), ("t1.s2_for_b1".to_owned(), 2)]));
        assert_eq!(state_mapping, HashMap::from([("a1.is_empty".to_owned(), 0), ("b1.is_empty".to_owned(), 1)]));
        assert_eq!(blackboard_mapping, HashMap::new());
        assert_eq!(operator_mapping, HashMap::from([("ops1".to_owned(), 0), ("ops2".to_owned(), 1)]));
        assert_eq!(tasks, vec![
            Task{
                preconditions: vec![Push(B(true))], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Composite(vec![1, 2]), 
                effects: vec![], 
                planning: vec![],
                is_method: false,
                wants: HashMap::new(),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(0)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(0, 0)]), 
                effects: vec![], 
                planning: vec![],
                is_method: true,
                wants: HashMap::from([(0, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(1)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(1, 0)]), 
                effects: vec![], 
                planning: vec![],
                is_method: true,
                wants: HashMap::from([(1, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            }
        ]);
    }

    #[test]
    fn test_planning() {
        let code = "task t1:\n\top()\nplanning:\n\tpop()";
        let mut state_mapping = HashMap::new();
        let mut blackboard_mapping = HashMap::new();
        let mut task_mapping = HashMap::new();
        let mut operator_mapping = HashMap::new();
        let mut type_mapping = HashMap::new();
        let mut compiler = DomainCompiler::new(&mut blackboard_mapping, &mut task_mapping, &mut operator_mapping, &mut type_mapping, &mut state_mapping);
        while compiler.pass < DomainCompiler::MAX_PASSES {
            for stmt in Parser::new(code) {
                stmt.expect("Unexpected parsing error").accept(&mut compiler).expect("Unexpected compilation error in unit-tests");
            }
            compiler.pass += 1;
        }

        let tasks = compiler.finish();
        assert_eq!(task_mapping, HashMap::from([("t1".to_owned(), 0)]));
        assert_eq!(state_mapping, HashMap::new());
        assert_eq!(blackboard_mapping, HashMap::new());
        assert_eq!(operator_mapping, HashMap::from([("pop".to_owned(), 0), ("op".to_owned(), 1)]));
        assert_eq!(tasks, vec![
            Task{
                preconditions: vec![Push(B(true))], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(1,0)]), 
                effects: vec![], 
                planning: vec![CallOperator(0, 0)],
                is_method: false,
                wants: HashMap::new(),
                provides: HashMap::new(),
            },
        ]);
    }

    #[test]
    fn test_typed_planning_parent_task() {
        let code = "task t1:\n\ttask s1(cell.is_empty) for Cell as cell:\n\t\tops1()\n\ttask s2(block.is_empty) for Block as block:\n\t\tops2()\nplanning:\n\tpop()";
        let mut state_mapping = HashMap::new();
        let mut blackboard_mapping = HashMap::new();
        let mut task_mapping = HashMap::new();
        let mut operator_mapping = HashMap::new();
        let mut type_mapping = HashMap::from([("Cell".to_owned(), vec!["a1".to_owned(), "a2".to_owned()]), ("Block".to_owned(), vec!["b1".to_owned(), "b2".to_owned()])]);
        let mut compiler = DomainCompiler::new(&mut blackboard_mapping, &mut task_mapping, &mut operator_mapping, &mut type_mapping, &mut state_mapping);
        while compiler.pass < DomainCompiler::MAX_PASSES {
            for stmt in Parser::new(code) {
                stmt.expect("Unexpected parsing error").accept(&mut compiler).expect("Unexpected compilation error in unit-tests");
            }
            compiler.pass += 1;
        }

        let tasks = compiler.finish();
        assert_eq!(task_mapping, HashMap::from([("t1".to_owned(), 0), ("t1.s1_for_a1".to_owned(), 1), ("t1.s1_for_a2".to_owned(), 2), ("t1.s2_for_b1".to_owned(), 3), ("t1.s2_for_b2".to_owned(), 4)]));
        assert_eq!(state_mapping, HashMap::from([("a1.is_empty".to_owned(), 0), ("a2.is_empty".to_owned(), 1), ("b1.is_empty".to_owned(), 2), ("b2.is_empty".to_owned(), 3)]));
        assert_eq!(blackboard_mapping, HashMap::new());
        assert_eq!(operator_mapping, HashMap::from([("pop".to_owned(), 0), ("ops1".to_owned(), 1), ("ops2".to_owned(), 2)]));
        assert_eq!(tasks, vec![
            Task{
                preconditions: vec![Push(B(true))], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Composite(vec![1, 2, 3, 4]), 
                effects: vec![], 
                planning: vec![CallOperator(0, 0)],
                is_method: false,
                wants: HashMap::new(),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(0)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(1, 0)]), 
                effects: vec![], 
                planning: vec![],
                is_method: true,
                wants: HashMap::from([(0, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(1)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(1, 0)]), 
                effects: vec![], 
                planning: vec![],
                is_method: true,
                wants: HashMap::from([(1, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(2)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(2, 0)]), 
                effects: vec![], 
                planning: vec![],
                is_method: true,
                wants: HashMap::from([(2, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(3)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(2, 0)]), 
                effects: vec![], 
                planning: vec![],
                is_method: true,
                wants: HashMap::from([(3, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            },
        ]);
    }

    #[test]
    fn test_typed_planning_methods() {
        let code = "task t1:\n\ttask s1(cell.is_empty) for Cell as cell:\n\t\tops1()\n\tplanning:\n\t\tpops1()\n\ttask s2(block.is_empty) for Block as block:\n\t\tops2()\n\tplanning:\n\t\tpops2()";
        let mut state_mapping = HashMap::new();
        let mut blackboard_mapping = HashMap::new();
        let mut task_mapping = HashMap::new();
        let mut operator_mapping = HashMap::new();
        let mut type_mapping = HashMap::from([("Cell".to_owned(), vec!["a1".to_owned(), "a2".to_owned()]), ("Block".to_owned(), vec!["b1".to_owned(), "b2".to_owned()])]);
        let mut compiler = DomainCompiler::new(&mut blackboard_mapping, &mut task_mapping, &mut operator_mapping, &mut type_mapping, &mut state_mapping);
        while compiler.pass < DomainCompiler::MAX_PASSES {
            for stmt in Parser::new(code) {
                stmt.expect("Unexpected parsing error").accept(&mut compiler).expect("Unexpected compilation error in unit-tests");
            }
            compiler.pass += 1;
        }

        let tasks = compiler.finish();
        assert_eq!(task_mapping, HashMap::from([("t1".to_owned(), 0), ("t1.s1_for_a1".to_owned(), 1), ("t1.s1_for_a2".to_owned(), 2), ("t1.s2_for_b1".to_owned(), 3), ("t1.s2_for_b2".to_owned(), 4)]));
        assert_eq!(state_mapping, HashMap::from([("a1.is_empty".to_owned(), 0), ("a2.is_empty".to_owned(), 1), ("b1.is_empty".to_owned(), 2), ("b2.is_empty".to_owned(), 3)]));
        assert_eq!(blackboard_mapping, HashMap::new());
        assert_eq!(operator_mapping, HashMap::from([("pops1".to_owned(), 0), ("ops1".to_owned(), 1), ("pops2".to_owned(), 2), ("ops2".to_owned(), 3)]));
        assert_eq!(tasks, vec![
            Task{
                preconditions: vec![Push(B(true))], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Composite(vec![1, 2, 3, 4]), 
                effects: vec![], 
                planning: vec![],
                is_method: false,
                wants: HashMap::new(),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(0)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(1, 0)]), 
                effects: vec![], 
                planning: vec![CallOperator(0, 0)],
                is_method: true,
                wants: HashMap::from([(0, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(1)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(1, 0)]), 
                effects: vec![], 
                planning: vec![CallOperator(0, 0)],
                is_method: true,
                wants: HashMap::from([(1, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(2)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(3, 0)]), 
                effects: vec![], 
                planning: vec![CallOperator(2, 0)],
                is_method: true,
                wants: HashMap::from([(2, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            },
            Task{
                preconditions: vec![ReadState(3)], 
                cost: vec![Push(I(0))], 
                body: TaskBody::Primitive(vec![CallOperator(3, 0)]), 
                effects: vec![], 
                planning: vec![CallOperator(2, 0)],
                is_method: true,
                wants: HashMap::from([(3, Inertia::Item(B(true)))]),
                provides: HashMap::new(),
            },
        ]);
    }

}