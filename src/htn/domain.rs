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


#[derive(Clone, Debug)]
pub struct Method {
    pub name: String,
    pub preconditions:Option<Expr>, 
    pub dependencies:HashSet<String>,
    pub body:Vec<Expr>, 
    neighbors: Vec<String>,
    effects: Vec<Expr>,
}


impl<'a> LinkedNode<'a> for Method {
    fn preconditions(&'a self) -> &'a Option<Expr> {
        &self.preconditions
    }

    fn cost(&'a self) -> i32 {
        5
    }

    fn effects(&'a self) -> &'a Vec<Expr> {
        &self.effects
    }

    fn neighbors(&'a self) -> Iter<String> {
        self.neighbors.iter()
    }
}

#[derive(Debug, Clone)]
pub enum TaskStatement {
    Composite(Vec<Method>),
    Primitive(Vec<Expr>)
}


#[derive(Debug, Clone)]
pub struct Task {
    pub name:String,
    pub preconditions:Option<Expr>, 
    pub dependencies:HashSet<String>,
    pub body:TaskStatement, 
    pub effects:Vec<Expr>,
    pub affects:HashSet<String>,
    neighbors: Vec<String>
}

impl<'a> LinkedNode<'a> for Task {
    fn preconditions(&'a self) -> &'a Option<Expr> {
        &self.preconditions
    }

    fn effects(&'a self) -> &'a Vec<Expr> {
        &self.effects
    }

    fn cost(&'a self) -> i32 {
        5
    }

    fn neighbors(&'a self) -> Iter<String> {
        self.neighbors.iter()
    }
}


impl std::hash::Hash for Task {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

impl std::cmp::PartialEq for Task {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name 
    }
}

impl std::cmp::Eq for Task {

}

enum ExpressionType {
    Call(String),
    Assignment(String)
}

enum BuildContext {
    Task(String, Option<TaskStatement>),
    Method(Vec<Expr>),
    Effects(Vec<Expr>, HashSet<String>),
    Preconditions(Option<Expr>, HashSet<String>),
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
        f.debug_struct("Domain").field("tasks", &self.tasks).field("world_variables", &self.world_variables).field("blackboard_variables", &self.blackboard_variables).field("operators", &self.operators).finish()
    }
}


impl Domain {
    fn build_task(&mut self, context: &mut Option<BuildContext>, name:String, preconditions:Option<Expr>, body:Box<Stmt>, effects:Option<Box<Stmt>>) -> Result<(), DomainError>  {
        if context.is_some() {
            return Err(DomainError{message:format!("Nested task definition is not allowed.")});
        }
        self.operators.remove(&name);
        let mut task_build_context = Some(BuildContext::Task(name.clone(), None));
        self.process_stmt(&mut task_build_context, &body)?;
        if let Some(BuildContext::Task(tname, Some(body))) = task_build_context {
            if tname != name {
                return Err(DomainError{message:format!("Task build context got overwritten")});
            }
            
            let (preconditions, dependencies) = if let Some(ref expr) = preconditions {
                let mut preconditions_build_context = Some(BuildContext::Preconditions(Some(expr.clone()), HashSet::new()));
                self.process_expr(&mut preconditions_build_context, expr)?;
                if let Some(BuildContext::Preconditions(r, e)) = preconditions_build_context {
                    (r, e)
                } else {
                    (None, HashSet::new())
                }
            } else {
                (None, HashSet::new())
            };
            let (effects, affects) = if let Some(stmt) = effects {
                let mut effects_build_context = Some(BuildContext::Effects(Vec::new(), HashSet::new()));
                self.process_stmt(&mut effects_build_context, &stmt)?;
                if let Some(BuildContext::Effects(r, e)) = effects_build_context {
                    (r, e)
                } else {
                    (Vec::new(), HashSet::new())
                }
            } else {
                (Vec::new(), HashSet::new())
            };
            self.tasks.insert(name.clone(), Task{name, preconditions, dependencies, body, effects, affects, neighbors:Vec::new()});
        } else {
            return Err(DomainError{message:format!("Task build context got overwritten")});
        }

        Ok(())

    }
    fn build_method(&mut self, context: &mut Option<BuildContext>, name:String, preconditions:Option<Expr>, body:Box<Stmt>) -> Result<(), DomainError>  {
        use TaskStatement::*;
        if let Some(BuildContext::Task(task_name, task_type)) = context {
            let mut build_context = Some(BuildContext::Method(Vec::new()));
            self.process_stmt(&mut build_context, &body)?;
            let body = if let Some(BuildContext::Method(body)) = build_context {
                body
            } else {
                panic!("Build context got everwritten");
            };
            let (preconditions, dependencies) = if let Some(ref expr) = preconditions {
                let mut preconditions_build_context = Some(BuildContext::Preconditions(Some(expr.clone()), HashSet::new()));
                self.process_expr(&mut preconditions_build_context, expr)?;
                if let Some(BuildContext::Preconditions(r, e)) = preconditions_build_context {
                    (r, e)
                } else {
                    (None, HashSet::new())
                }
            } else {
                (None, HashSet::new())
            };
            let me = Method{name, preconditions, dependencies, body, neighbors:Vec::new(), effects:Vec::new()};
            match task_type {
                Some(Primitive(_)) => return Err(DomainError{message:format!("Tasks can be either composite or primitive. Task {} is both.", task_name)}),
                Some(Composite(method_vec)) => method_vec.push(me),
                None => *task_type = Some(Composite(vec![me])),
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
        let mut new_context: &mut Option<BuildContext> = &mut None;
        if let Some(build_context) = context {
            match build_context {
                Task(task_name, task_type) => match task_type {
                    Some(Composite(_)) => return Err(DomainError{message:format!("Tasks can be either composite or primitive. Task {} is both.", task_name)}),
                    Some(Primitive(expr_vec)) => expr_vec.push(expr.clone()),
                    None => *task_type = Some(Primitive(vec!(expr.clone()))),
                }
                Method(method_body) => method_body.push(expr.clone()),
                _ => new_context = context,
            }
        } else {
            new_context = context;
        }
        Ok(match expr {
            Binary(left, _, right) => {self.process_expr(new_context, left.as_ref())?; self.process_expr(context, right.as_ref())?},
            Grouping(g) => self.process_expr(new_context, g.as_ref())?,
            Literal(_) => (),
            Variable(var_name) => match new_context {
                Some(Expression(ExpressionType::Call(v))) => v.insert_str(0,var_name), 
                Some(Preconditions(_, set)) => {set.insert(var_name.clone());},
                _ => (),
            },
            Unary(_, right) => self.process_expr(new_context, right)?,
            Assignment(var_name, right) => { match right.as_ref() {
                Call(_, _, _) => self.blackboard_variables.insert(var_name.clone()),
                _ => self.world_variables.insert(var_name.clone()),
            };
            match new_context {
                Some(Effects(e_vec,set)) => {e_vec.push(expr.clone()); set.insert(var_name.clone());},
                _ => (),
            }
            self.process_expr(new_context, right)?;
            
            },
            Call(target, _, args) => {
                if !self.tasks.contains_key(target) {
                    self.operators.insert(target.clone());
                }
                for e in args {
                    self.process_expr(new_context, e)?;
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

    fn optimize(&mut self) {
        let tasks = self.tasks.clone();
        for task_name in tasks.keys() {
            if let Some(ref task) = tasks.get(task_name) {
                for (other_name, other) in self.tasks.iter_mut() {
                    if task_name != other_name {
                        // if task effects other's preconditions
                        if task.affects.intersection(&other.dependencies).count() > 0 {
                            other.neighbors.push(task_name.clone());
                            // task.neighbors.push(other_name.clone());
                            println!("{} enables {}", task_name, other_name);
                        }
                    }
                }
            }
        }
        
    }

    pub fn cost(&self, from:&String, to:&String) -> i32 {
        4
    }

    pub fn from_file(filepath:&str) -> Result<Domain, DomainError> {
        let htn_source = fs::read_to_string(filepath).expect("File error:");
        let (ast, errors) = Parser::parse(htn_source.as_str());
        Parser::print_parse_errors(errors, htn_source.as_str(), filepath);
        let mut domain = Domain{tasks: HashMap::new(), world_variables:HashSet::new(), blackboard_variables:HashSet::new(), operators:HashSet::new()};
        domain.pass(&ast)?;
        domain.optimize();
        // domain.tasks.iter().for_each(|(k, t)| println!("Task {} effects: {:?}, depends: {:?}", k, t.affects, t.dependencies));
        Ok(domain)
    }
}