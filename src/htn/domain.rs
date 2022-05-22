use std::{fs, fmt, rc::{Weak, Rc}, hash::Hash, collections::{HashSet, HashMap}};
use core::slice::Iter;
use super::{parser::{Parser, Stmt, Expr}, search::LinkedNode};


pub struct DomainError {
    message: String,
}

impl fmt::Debug for DomainError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Debug)]
pub enum TaskStatement {
    Composite(Vec<String>),
    Primitive(Vec<Expr>)
}

#[derive(Debug)]
pub struct Task {
    preconditions:Option<Expr>, 
    body:TaskStatement, 
    effects:Option<Box<Stmt>>,
    neighbors: Vec<String>
}


struct TaskIterator<'a> {
    iter: Iter<'a, String>
}

impl<'a> Iterator for TaskIterator<'a> {
    type Item = &'a String;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

impl<'a> LinkedNode<'a, Task, TaskIterator<'a>> for Task {
    fn neighbors(&self) -> TaskIterator {
        TaskIterator{iter:self.neighbors.iter()}
        
    }
}

enum ExpressionType {
    Call(String),
    Assignment(String)
}

enum BuildContext {
    Task(String, Option<TaskStatement>),
    Expression(ExpressionType),
}

pub struct Domain {
    pub tasks: HashMap<String, Task>,
    pub world_variables: HashSet<String>,
    pub blackboard_variables: HashSet<String>,
    pub operators: HashSet<String>,
}

impl std::fmt::Debug for Domain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Domain").field("tasks", &self.tasks.keys()).field("world_variables", &self.world_variables).field("blackboard_variables", &self.blackboard_variables).field("operators", &self.operators).finish()
    }
}

impl Domain {
    fn build_task(&mut self, context: &mut Option<BuildContext>, name:String, preconditions:Option<Expr>, body:Box<Stmt>, effects:Option<Box<Stmt>>) -> Result<(), DomainError>  {
        if context.is_some() {
            return Err(DomainError{message:format!("Nested task definition is not allowed.")});
        }
        self.operators.remove(&name);
        let mut build_context = Some(BuildContext::Task(name.clone(), None));
        self.process_stmt(&mut build_context, &body)?;
        if let Some(BuildContext::Task(tname, Some(t))) = build_context {
            if tname != name {
                return Err(DomainError{message:format!("Task build context got overwritten")});
            }
            self.tasks.insert(name.clone(), Task{preconditions:preconditions.clone(), body:t, effects:effects.clone(), neighbors:Vec::new()});
        } else {
            return Err(DomainError{message:format!("Task build context got overwritten")});
        }
        if let Some(expr) = preconditions {
            self.process_expr(&mut None, &expr)?;
        }
        if let Some(stmt) = effects {
            self.process_stmt(&mut None, &stmt)?;
        }
        Ok(())

    }
    fn build_method(&mut self, context: &mut Option<BuildContext>, name:String, preconditions:Option<Expr>, body:Box<Stmt>) -> Result<(), DomainError>  {
        use TaskStatement::*;
        if let Some(BuildContext::Task(task_name, task_type)) = context {
            match task_type {
                Some(Primitive(_)) => return Err(DomainError{message:format!("Tasks can be either composite or primitive. Task {} is both.", task_name)}),
                None => *task_type = {let methods_vec = vec![name]; Some(Composite(methods_vec))},
                Some(Composite(methods_vec)) => {
                    let newtask_name = format!("{}_method{}", task_name, name);
                    methods_vec.push(newtask_name.clone());
                    let mut build_context = Some(BuildContext::Task(newtask_name, Some(Primitive(Vec::new()))));
                    self.process_stmt(&mut build_context, &body)?;
                    if let Some(BuildContext::Task(nn, Some(ntb))) = build_context {
                        self.tasks.insert(nn, Task{preconditions, body:ntb, effects:None, neighbors:Vec::new()});
                    }
                },
            }
            Ok(())
        } else {
            return Err(DomainError{message:format!("Method declaration outside of task body is not allowed.")});
        }
        

    }
    fn process_expr(&mut self, context:&mut Option<BuildContext>, expr:&Expr) -> Result<(), DomainError>  {
        use Expr::*;
        use BuildContext::*;
        use TaskStatement::*;
        if let Some(Task(task_name, task_type)) = context {
            match task_type {
                Some(Composite(_)) => return Err(DomainError{message:format!("Tasks can be either composite or primitive. Task {} is both.", task_name)}),
                Some(Primitive(expr_vec)) => expr_vec.push(expr.clone()),
                None => *task_type = Some(Primitive(Vec::new())),
            }
        }
        Ok(match expr {
            Binary(left, _, right) => {self.process_expr(context, left.as_ref())?; self.process_expr(context, right.as_ref())?},
            Grouping(g) => self.process_expr(context, g.as_ref())?,
            Literal(_) => (),
            Variable(var_name) => match context {
                Some(Expression(ExpressionType::Call(v))) => v.insert_str(0,var_name), 
                _ => (),
            },
            Unary(_, right) => self.process_expr(context, right)?,
            Assignment(var_name, right) => { match right.as_ref() {
                Call(_, _, _) => self.blackboard_variables.insert(var_name.clone()),
                _ => self.world_variables.insert(var_name.clone()),
            };
            },
            Call(left, _, args) => {
                let mut build_context = Some(Expression(ExpressionType::Call(String::new())));
                self.process_expr(&mut build_context, left)?; 
                if let Some(Expression(ExpressionType::Call(call_target))) = build_context {
                    if !self.tasks.contains_key(&call_target) {
                        self.operators.insert(call_target);
                    }
                }
                for e in args {
                    self.process_expr(context, e)?;
                }
            },
        })
    }

    fn process_stmt(&mut self, context:&mut Option<BuildContext>, stmt:&Stmt) -> Result<(), DomainError>  {
        match stmt {
            Stmt::Method(name, preconditions, body) => self.build_method(context, name.clone(), preconditions.clone(), body.clone()),
            Stmt::Task(name, preconditions, body, effects) => {
                self.build_task(context, name.clone(), preconditions.clone(), body.clone(), effects.clone())
            },
            Stmt::Block(sub) => Ok(for item in sub { self.process_stmt(context, item)?} ),
            Stmt::Expression(expr) => self.process_expr(context, &expr),
        }
    }

    fn pass(&mut self, ast:&Vec<Stmt>) -> Result<(), DomainError> {
        let mut build_context = None;
        for stmt in ast {
            self.process_stmt(&mut build_context, stmt)?;
        }
        Ok(())
    }


    pub fn from_file(filepath:&str) -> Result<Domain, DomainError> {
        let htn_source = fs::read_to_string(filepath).expect("File error:");
        let (ast, errors) = Parser::parse(htn_source.as_str());
        Parser::print_parse_errors(errors, htn_source.as_str(), filepath);
        let mut domain = Domain{tasks: HashMap::new(), world_variables:HashSet::new(), blackboard_variables:HashSet::new(), operators:HashSet::new()};
        domain.pass(&ast)?;
        println!("{:?}", domain);
        Ok(domain)
    }
}