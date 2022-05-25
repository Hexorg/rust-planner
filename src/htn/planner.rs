use std::{collections::HashMap, ops::Deref};

use crate::htn::search::Astar;

use super::{domain::Domain, parser::{Expr, TokenData}};

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
    blackboard: HashMap<String, i32>
}

#[derive(Debug)]
enum ExpressionResult {
    Literal(i32), 
    Task(String),
    Variable(String),
    FunctionCall(String)
}

impl Planner<'_>{

    fn run_call(&mut self, expr:&Expr) {
        if let Expr::Call(func, _, args) = expr {
            let result = args.iter().map(|a| a.eval(&self.blackboard));
            if result.len() == 0 {
                if self.domain.tasks.contains_key(func) {
                    println!("Calling task {}", func);
                    self.run_task(func)
                } else {
                    println!("Calling operator {}()", func);
                }
            } else {
                println!("Calling operator {}({:?})", func, result);
            }

        } else {
            panic!("Unexpected expression to call: {:?}", expr);
        }
    }

    fn run_exprs(&mut self, exprs:&Vec<Expr>, can_call:bool) {
        for expr in exprs {
            match expr {
                Expr::Assignment(var, e) => {
                    if let Expr::Call(_,_,_) = e.as_ref() {
                        self.run_call(e);
                    } else {
                        self.state.s.insert(var.clone(), e.eval(&self.state.s));

                    }
                },
                Expr::Call(_, _, _) => if can_call { self.run_call(expr)} else { panic!("Unexpected call expression.")},
                _ => panic!("Unexpected expression in the task definition.")
            }
        }

    }
 

    fn run_task(&mut self, task_name: &str) {
        let task = self.domain.tasks.get(task_name).unwrap();
        if if let Some(ref preconditions) = task.preconditions {
            // println!("Checking task's {} preconditions {:?}", task_name, preconditions);
            preconditions.eval(&self.state.s) == 1    
        } else {
            true
        } {
            // println!("Running task {}", task_name);
            match &task.body {
                super::domain::TaskStatement::Composite(ref methods) => {
                    let mut is_run_method = false;
                    for method in methods {
                        if if let Some(ref preconditions) = method.preconditions {
                            let r = preconditions.eval(&self.state.s);
                            r == 1
                        } else {
                            true
                        } {
                            println!("Running method {}.{}", task_name, method.name);
                            is_run_method = true;
                            self.run_exprs(&method.body, true);
                            break
                        }
                    }
                    if !is_run_method {
                        println!("No methods can run when state is {:?}", self.state);
                        for method in methods {
                        let start = State{s:self.state.s.clone(), last_task_name:method.name.clone()};
                            if let Some(plan) = Astar(start, method, |f| 4, self.domain) {
                                println!("We can achieve {}.{} through {:?}", task_name, method.name, plan);
                                is_run_method = true;
                                plan.iter().for_each(|task| self.run_task(task));
                                break
                            }
                        }
                    }
                    if is_run_method && task.effects.len() > 0{
                        self.run_exprs(&task.effects, false);
                    }
                    if !is_run_method {
                        panic!("Unable to find any solutions to run {}", task_name);
                    }
                },
                super::domain::TaskStatement::Primitive(body) => { println!("Running primitive task {}", task_name); self.run_exprs(body, true)},
            }
            self.run_exprs(&task.effects, false)
        }
    }
    // fn run_main(&mut self) {
    //     self.run_stmt(self.ast.get(self.main_id).unwrap())
    // }
    pub fn run<'a>(domain: &'a Domain) {
        let mut state = State{s:HashMap::new(), last_task_name:String::from("start")};
        for var in &domain.world_variables {
            state.s.insert(var.clone(), 0);
        }
        state.s.insert(String::from("isHungry"), 1);
        let mut planner = Planner{domain, state, blackboard:HashMap::new()};
        planner.run_task("Main");
    }
}