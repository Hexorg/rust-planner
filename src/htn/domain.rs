use std::{fs, fmt, rc::{Weak, Rc}};
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

enum TaskType {
    COMPOSITE,
    PRIMITIVE
}

struct Task {
    t: TaskType,
    stmt:Rc<Stmt>,
    neighbors: Vec<Rc<Task>>
}

struct TaskIterator<'a> {
    iter: Iter<'a, Rc<Task>>
}

impl<'a> Iterator for TaskIterator<'a> {
    type Item = &'a Rc<Task>;

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
    Task(String, Option<TaskType>),
    Expression(ExpressionType)
}


pub struct Domain {
    pub tasks: Vec<Task>,
    pub world_variables: Vec<String>,
    pub blackboard_variables: Vec<String>,
    pub operators: Vec<String>,
}

impl Domain {
    fn build_task(&mut self, context: &mut Vec<BuildContext>, name:&String, preconditions:&Option<Expr>, body:&Rc<Stmt>, effects:&Option<Rc<Stmt>>) -> Result<(), DomainError>  {
        let mut build_context = Some(BuildContext::Task(name.clone(), None));

        self.process_stmt(&mut build_context, body)

    }
    fn build_method(&mut self, context: &mut Vec<BuildContext>, name:&String, preconditions:&Option<Expr>, body:&Rc<Stmt>) -> Result<(), DomainError>  {
        if let Some(BuildContext::Task(task_name, tt)) = context {
            match tt {
                Some(TaskType::PRIMITIVE) => return Err(DomainError{message:format!("Tasks can be either composite or primitive. Task {} is both.", task_name)}),
                None => *tt = Some(TaskType::COMPOSITE),
                _ => (),
            }
        }
        self.process_stmt(context, body)
    }
    fn process_expr(&mut self, context:&mut Vec<BuildContext>, expr:&Expr) -> Result<(), DomainError>  {
        Ok(match expr {
            Expr::Binary(left, _, right) => {self.process_expr(context, left.as_ref())?; self.process_expr(context, right.as_ref())?},
            Expr::Grouping(g) => self.process_expr(context, g.as_ref())?,
            Expr::Literal(_) => (),
            Expr::Variable(var_name) => match context {
                Some(BuildContext::Expression(ExpressionType::Call(v))) => v.insert_str(0,var_name), 
                _ => (),
            },
            Expr::Unary(_, right) => self.process_expr(context, right)?,
            Expr::Assignment(_, right) => {let mut build_context = Some(BuildContext::Expression(ExpressionType::Assignment(String::new()))); 
                self.process_expr(&mut build_context, right)?
            },
            Expr::Call(left, _, args) => {let mut build_context = Some(BuildContext::Expression(ExpressionType::Call(String::new()))); 
                self.process_expr(&mut build_context, left)?; 
                for e in args {
                    self.process_expr(context, e)?;
                }
            },
        })
    }

    fn process_stmt(&mut self, context:&mut Vec<BuildContext>, stmt:&Stmt) -> Result<(), DomainError>  {
        match stmt {
            Stmt::Method(name, preconditions, body) => self.build_method(context, name, preconditions, body),
            Stmt::Task(name, preconditions, body, effects) => self.build_task(context, name, preconditions, body, effects),
            Stmt::Block(sub) => Ok(for item in sub.iter() { self.process_stmt(context, item)?} ),
            Stmt::Expression(expr) => {let mut build_context = None; self.process_expr(&mut build_context, &expr)},
        }
    }

    fn pass(&mut self, ast:Vec<Stmt>) -> Result<(), DomainError> {
        let mut build_context = Vec::new();
        for stmt in ast {
            self.process_stmt(&mut build_context, &stmt)?;
        }
        Ok(())
    }

    pub fn from_file(filepath:&str) -> Result<Domain, DomainError> {
        let htn_source = fs::read_to_string(filepath).expect("File error:");
        let (ast, errors) = Parser::parse(htn_source.as_str());
        Parser::print_parse_errors(errors, htn_source.as_str(), filepath);
        let mut domain = Domain{ tasks:Vec::new(), world_variables:Vec::new(), blackboard_variables:Vec::new(), operators:Vec::new()};
        domain.pass(ast)?;
        Ok(domain)
    }
}