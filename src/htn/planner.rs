use std::{collections::HashMap, ops::Deref};

use crate::htn::search::Astar;

use super::{domain::Domain, parser::{Expr, TokenData, ParserError}};

#[derive(Debug, Clone)]
pub struct State {
    pub s: HashMap<String, i32>,
    pub last_task_name: String
}

impl std::hash::Hash for State {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.s.values().for_each(|val| val.hash(state))
    }
}
impl std::cmp::PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        let mut result = false;
        self.s.values().zip(other.s.values()).fold(&mut result, |acc, (left, right)| {*acc &=  left == right; acc});
        result
    }
}
impl std::cmp::Eq for State {

}
pub struct Planner<'a> {
    domain: &'a Domain,
    state: State,
    blackboard: HashMap<String, i32>,
    plan: Vec<String>
}

#[derive(Debug)]
enum ExpressionResult {
    Literal(i32), 
    Task(String),
    Variable(String),
    FunctionCall(String)
}

impl Planner<'_>{

    fn run_call(&mut self, expr:&Expr) -> Result<(), ParserError> {
        // if self.plan.len() > 32 {
        //     Ok(())
        // } else {
            if let Expr::Call(func, _, args) = expr {
                if args.len() == 0 {
                    if self.domain.tasks.contains_key(func) {
                        // println!("Calling task {}", func);
                        self.run_task(func)?
                    } else {
                        // println!("Calling operator {}()", func);
                        self.plan.push(func.clone());
                    }
                } else {
                    // println!("Calling operator {}(args)", func);
                    // let arglist = args.fold(String::new(), |mut acc, item| {acc += &format!("{}", item); acc});
                    self.plan.push(format!("{}({})", func, args.iter().fold(String::new(), |mut acc, item| {acc += &format!("{}, ", item); acc})));
                }
                Ok(())
            } else {
                Err(expr.to_err(String::from("Attempted to call an unexpected expression")))
            }
        // }
    }

    fn run_exprs(&mut self, exprs:&Vec<Expr>, can_call:bool) -> Result<(), ParserError> {
        for expr in exprs {
            match expr {
                Expr::Assignment(var, e, _) => {
                    if let Expr::Call(_,_,_) = e.as_ref() {
                        self.run_call(e)?;
                        self.plan.push(format!("Store previous result as {}", var));
                    } else {
                        self.state.s.insert(var.clone(), e.eval(&self.state.s)?);
                    }
                    Ok(())
                },
                Expr::Call(_, _, _) => if can_call { self.run_call(expr)} else { Err(expr.to_err(String::from("Unexpected call expression."))) },
                _ => Err(expr.to_err(String::from("Unexpected expression in the task definition.")))
            }?
        }
        Ok(())
    }
 

    fn run_task(&mut self, task_name: &str) -> Result<(), ParserError> {
        let task = self.domain.tasks.get(task_name).expect(format!("Unknown task name {}", task_name).as_str());
        if if let Some(ref preconditions) = task.preconditions {
            // println!("Checking task's {} preconditions {:?}", task_name, preconditions);
            preconditions.eval(&self.state.s)? == 1    
        } else {
            true
        } {
            println!("Running task {}", task_name);
            match &task.body {
                super::domain::TaskStatement::Composite(ref methods) => {
                    let mut is_run_method = false;
                    for method in methods {
                        if if let Some(ref preconditions) = method.preconditions {
                            let r = preconditions.eval(&self.state.s)?;
                            r == 1
                        } else {
                            true
                        } {
                            println!("Running method {}.{}", task_name, method.name);
                            is_run_method = true;
                            self.run_exprs(&method.body, true)?;
                            break
                        }
                    }
                    if !is_run_method {
                        // println!("No methods can run when state is {:?}", self.state);
                        for method in methods {
                            let start = State{s:self.state.s.clone(), last_task_name:method.name.clone()};
                            if let Some(plan) = Astar(start, method, |f| 4, self.domain) {
                                println!("We can achieve {}.{} through {:?}", task_name, method.name, plan);
                                is_run_method = true;
                                plan.iter().try_for_each(|task| self.run_task(task))?;
                                break
                            }
                        }
                    }
                    if is_run_method && task.effects.len() > 0{
                        self.run_exprs(&task.effects, false)?;
                    }
                    if !is_run_method {
                        panic!("Unable to find any solutions to run {}", task_name);
                    }
                },
                super::domain::TaskStatement::Primitive(body) => { self.run_exprs(body, true)?},
            }
            self.run_exprs(&task.effects, false)?
        } else {
            panic!("Task {} preconditions are unmet!", task_name);
        }
        Ok(())
    }
    // fn run_main(&mut self) {
    //     self.run_stmt(self.ast.get(self.main_id).unwrap())
    // }
    pub fn run<'a>(domain: &'a Domain) -> Result<Vec<String>, ParserError> {
        let mut state = State{s:HashMap::new(), last_task_name:String::from("start")};
        for var in &domain.world_variables {
            state.s.insert(var.clone(), 0);
        }
        state.s.insert(String::from("WsCanSeeEnemy"), 1);
        let mut planner = Planner{domain, state, blackboard:HashMap::new(), plan:Vec::new()};
        planner.run_task("Main")?;
        Ok(planner.plan)
    }
}