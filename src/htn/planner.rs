use std::{collections::HashMap, rc::Rc};


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


 

    fn run_task(&mut self, task: &Stmt) -> Result<bool, ParserError> {
        if task.are_preconditions_satisfied(&self.state.0)? == 1 {
            if let Stmt::Task{name,..} = task {
                self.last_successfull_task = name.clone();
            }
            // println!("Running task {}", task.name()?);
            if task.is_composite()? {
                let mut is_method_run = false;
                task.for_each_method_while(&mut |method| if self.run_task(method).unwrap() { is_method_run = true; false } else { true });
                if !is_method_run { // no methods are statically satisfied
                    task.for_each_method_while(&mut |method| { // figure out which method can be reached through search
                        let start = StateAndPath{state:self.state.clone(), method_name:self.last_successfull_task.clone()};
                        if let Some(plan) = Astar(start, method, |f| 4, self.domain) {
                            if plan.len() > 0 {
                                is_method_run = true;
                                for subtask in plan {
                                    if !self.run_task(self.domain.tasks.get(&subtask).unwrap()).unwrap() {
                                        panic!("Planner thought task is achievable but it's not");
                                    }
                                }
                                self.run_task(method);
                                false 
                            } else { true }
                        } else {
                            true
                        }
                    });
                }
                if !is_method_run {
                    panic!("No solutions found to reach {} methods", task.name()?);
                }
                // println!("Done running composite task {}", task.name()?);
            } else {
                task.for_each_operator(&mut |op| {
                    let target = op.get_call_target().expect("Only call expressions are supported in task/method bodies.");
                    if let Some(task) = self.domain.tasks.get(&target) {
                        if !self.run_task(task).unwrap() {
                            let start = StateAndPath{state:self.state.clone(), method_name:self.last_successfull_task.clone()};
                            if let Some(plan) = Astar(start, task, |f| 4, self.domain) {
                                if plan.len() > 0 {
                                    for subtask in plan {
                                        if !self.run_task(self.domain.tasks.get(&subtask).unwrap()).unwrap() {
                                            panic!("Planner thought task is achievable but it's not");
                                        }
                                    }
                                }
                            }
                            self.run_task(task);
                        }
                    } else {
                        let op_type = if op.get_assignment_target().is_some() { "blackboard"} else { "simple" };
                        println!("Calling {} operator {}", op_type, op);
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
        state.0.insert(Rc::new(String::from("WsCanSeeEnemy")), 1);
        let mut planner = Planner{domain, state, blackboard:HashMap::new(), plan:Vec::new(), last_successfull_task:domain.main.clone()};
        planner.run_task(domain.tasks.get(&domain.main).expect("Unable to find Main task"));
        Ok(planner.plan)
    }
}