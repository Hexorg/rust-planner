use std::{collections::HashMap, rc::Rc, cmp::Reverse};


use priority_queue::PriorityQueue;

use super::{domain::Domain, search::{Astar, StateAndPath}, parser::{Stmt, Expr, ParserError}};

#[derive(Debug, Clone)]
pub struct State(pub HashMap<Rc<String>, i32>);

impl std::hash::Hash for State {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.values().for_each(|val| val.hash(state))
    }
}
impl std::cmp::PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        let mut result = false;
        self.0.values().zip(other.0.values()).fold(&mut result, |acc, (left, right)| {*acc &=  left == right; acc});
        result
    }
}
impl std::cmp::Eq for State {

}
pub struct Planner<'a> {
    domain: &'a Domain,
    state: State,
    last_successfull_task:Rc<String>,
    method_heatmap:HashMap<Rc<String>, i32>,
    plan: Vec<String>
}


impl Planner<'_>{

    fn plan_to_run_task(&mut self, task:&Stmt) -> Result<bool, ParserError> {
        let start = StateAndPath{state:self.state.clone(), method_name:self.last_successfull_task.clone()};
        if let Some(plan) = Astar(start, task, |f| 4, self.domain) {
            if plan.len() > 0 {
                for subtask in plan {
                    if !self.run_task(self.domain.tasks.get(&subtask).unwrap())? {
                        panic!("Planner thought task is achievable but it's not");
                    }
                }
                self.run_task(task)
            } else {
                Ok(false)
            }
        } else {
            Ok(false)
        }
    }
 

    fn run_task(&mut self, task: &Stmt) -> Result<bool, ParserError> {
        if self.plan.len() > 40 && task.name()? == self.domain.main {
            return Ok(true)
        }
        if task.are_preconditions_satisfied(&self.state.0)? == 1 {
            if let Stmt::Task{name,..} = task {
                self.last_successfull_task = name.clone();
            }
            // println!("Running task {}", task.name()?);
            if task.is_composite()? {
                let mut queue = PriorityQueue::new();
                task.for_each_method(&mut |method| {
                    let method_name = method.name().unwrap();
                    if method.are_preconditions_satisfied(&self.state.0).unwrap() == 1 {
                        queue.push(
                            method_name.clone(), 
                            Reverse(*self.method_heatmap.get(&method_name).or(Some(&0)).unwrap())
                        );
                    } else {
                        self.method_heatmap.insert(method.name().unwrap().clone(), -10);
                    }
                });
                if queue.len() == 0 { // no methods are statically satisfied
                    task.for_each_method_while(&mut |method| !self.plan_to_run_task(method).unwrap());
                } else {
                    if let Some((method_name, _)) = queue.pop() {
                        task.for_each_method_while(&mut |method| if method.name().unwrap() == method_name { self.method_heatmap.insert(method_name.clone(), self.method_heatmap.get(&method_name).or(Some(&0)).unwrap()+1);!self.run_task(method).unwrap()} else { true });
                    }
                }
                // println!("Done running composite task {}", task.name()?);
            } else {
                task.for_each_operator(&mut |op| {
                    let target = op.get_call_target().expect("Only call expressions are supported in task/method bodies.");
                    if let Some(task) = self.domain.tasks.get(&target) {
                        if !self.run_task(task).unwrap() {
                            if !self.plan_to_run_task(task).unwrap() {
                                panic!("No solutions found to run {} task", task.name().unwrap());
                            }
                        }
                    } else {
                        // let op_type = if op.get_assignment_target().is_some() { "blackboard"} else { "simple" };
                        // println!("Calling {} operator {}", op_type, op);
                        self.plan.push(format!("{}", op));
                    }
                });
                // println!("Done running primitive task {}", task.name()?);
            }
            task.effect(&mut self.state);
            Ok(true)
        } else {
            Ok(false)
        }
    }
    // fn run_main(&mut self) {
    //     self.run_stmt(self.ast.get(self.main_id).unwrap())
    // }
    pub fn run<'a>(domain: &'a Domain) -> Result<Vec<String>, ParserError> {
        let mut state = State(HashMap::new());
        for task in domain.tasks.values() {
            for var in task.affects().iter().chain(task.depends().iter()) {
                state.0.insert(var.clone(), 0);
            }

        }
        // state.0.insert(Rc::new(String::from("WsCanSeeEnemy")), 1);
        let mut planner = Planner{domain, state, plan:Vec::new(), method_heatmap:HashMap::new(), last_successfull_task:domain.main.clone()};
        planner.run_task(domain.tasks.get(&domain.main).expect("Unable to find Main task"));
        Ok(planner.plan)
    }
}