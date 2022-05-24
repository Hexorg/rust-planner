use std::{collections::HashMap, ops::Deref};

use crate::htn::search::Astar;

use super::{domain::Domain, parser::{Expr, TokenData}};

pub struct State (
    pub HashMap<String, i32>
);

impl std::hash::Hash for State {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.values().for_each(|val| val.hash(state))
    }
}
impl std::cmp::PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl std::cmp::Eq for State {

}
pub struct Planner<'a> {
    domain: &'a Domain,
    state: State,
}

#[derive(Debug)]
enum ExpressionResult {
    Literal(i32), 
    Task(String),
    Variable(String),
    FunctionCall(String)
}

impl Planner<'_>{

    fn run_exprs(&mut self, exprs:&Vec<Expr>) {

    }
 

    fn run_task(&mut self, task_name: &str) {
        let task = self.domain.tasks.get(task_name).unwrap();
        if if let Some(ref preconditions) = task.preconditions {
            preconditions.eval(&self.state.0) == 1    
        } else {
            true
        } {
            match &task.body {
                super::domain::TaskStatement::Composite(ref methods) => {
                    let mut is_run_method = false;
                    for method in methods {
                        println!("Method.name:{} ", method.name);
                        if if let Some(ref preconditions) = method.preconditions {
                            let r = preconditions.eval(&self.state.0);
                            r == 1
                        } else {
                            true
                        } {
                            is_run_method = true;
                            self.run_exprs(&method.body);
                        }
                    }
                    if !is_run_method {
                        panic!("Unable to run any methods");
                    }
                    if is_run_method && task.effects.len() > 0{
                        task.effects.iter().for_each(|e| self.run_task(e.is_call().unwrap()))
                    }
                },
                super::domain::TaskStatement::Primitive(body) => body.iter().for_each(|expr| self.run_task(expr.is_call().unwrap())),
            }
        }
    }
    // fn run_main(&mut self) {
    //     self.run_stmt(self.ast.get(self.main_id).unwrap())
    // }
    pub fn run<'a>(domain: &'a Domain) {
        let mut state = State(HashMap::new());
        for var in &domain.world_variables {
            state.0.insert(var.clone(), 0);
        }
        state.0.insert(String::from("isHungry"), 1);
        let mut planner = Planner{domain, state};
        planner.run_task("Main");
    }
}