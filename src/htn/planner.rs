use std::{collections::HashMap, rc::Rc, cmp::Reverse, ops::Deref};


use priority_queue::PriorityQueue;

use super::{domain::Domain, search::{Astar, StateAndPath}, parser::{Stmt, Expr, Error}};

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

#[derive(Debug)]
pub struct PlanStep {
    pub assignment: Option<Rc<String>>,
    pub operator: Rc<String>,
    pub arguments:Vec<Rc<String>>
}

impl From<&Expr> for PlanStep {
    fn from(expr: &Expr) -> Self {
        let assignment = if let Some(tgt) = expr.get_assignment_target() {
            Some(tgt.clone())
        } else {
            None
        };
        let operator = expr.get_call_target().unwrap().clone();
        let arguments = expr.get_call_arguments();
        Self{assignment, operator, arguments}
    }
}

pub struct Planner<'a> {
    domain: &'a Domain,
    state: State,
    last_successfull_task:Rc<String>,
    method_heatmap:HashMap<Rc<String>, i32>,
    plan: Vec<PlanStep>
}


impl Planner<'_>{

    fn plan_to_run_task(&mut self, task:&Stmt) -> Result<bool, Error> {
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
 

    fn run_task(&mut self, task: &Stmt) -> Result<bool, Error> {
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
                    let method_name = method.name()?;
                    if method.are_preconditions_satisfied(&self.state.0)? == 1 {
                        queue.push(
                            method_name.clone(), 
                            Reverse(*self.method_heatmap.get(&method_name).or(Some(&0)).unwrap())
                        );
                    } else {
                        self.method_heatmap.insert(method.name().unwrap().clone(), -10);
                    }
                    Ok(())
                })?;
                if queue.len() == 0 { // no methods are statically satisfied
                    task.for_each_method_while(&mut |method| Ok(!self.plan_to_run_task(method)?))?;
                } else {
                    if let Some((method_name, _)) = queue.pop() {
                        task.for_each_method_while(&mut |method| if method.name()? == method_name { self.method_heatmap.insert(method_name.clone(), self.method_heatmap.get(&method_name).or(Some(&0)).unwrap()+1);Ok(!self.run_task(method)?)} else { Ok(true) })?;
                    }
                }
                // println!("Done running composite task {}", task.name()?);
            } else {
                task.for_each_operator(&mut |op| {
                    let target = op.get_call_target().expect("Only call expressions are supported in task/method bodies.");
                    if let Some(task) = self.domain.tasks.get(&target) {
                        if !self.run_task(task)? {
                            if !self.plan_to_run_task(task)? {
                                panic!("No solutions found to run {} task", task.name()?);
                            }
                        }
                    } else {
                        // let op_type = if op.get_assignment_target().is_some() { "blackboard"} else { "simple" };
                        // println!("Calling {} operator {}", op_type, op);
                        self.plan.push(PlanStep::from(op));
                    }
                    Ok(())
                })?;
                // println!("Done running primitive task {}", task.name()?);
            };
            task.effect(&mut self.state);
            Ok(true)
        } else {
            Ok(false)
        }
    }
    // fn run_main(&mut self) {
    //     self.run_stmt(self.ast.get(self.main_id).unwrap())
    // }
    pub fn run<'a>(domain: &'a Domain, state:State) -> Result<Vec<PlanStep>, Error> {
        // let mut state = State(HashMap::new());
        // for task in domain.tasks.values() {
        //     for var in task.affects().iter().chain(task.depends().iter()) {
        //         state.0.insert(var.clone(), 0);
        //     }

        // }
        // state.0.insert(Rc::new(String::from("WsCanSeeEnemy")), 1);
        let mut planner = Planner{domain, state, plan:Vec::new(), method_heatmap:HashMap::new(), last_successfull_task:domain.main.clone()};
        planner.run_task(domain.tasks.get(&domain.main).expect("Unable to find Main task"))?;
        Ok(planner.plan)
    }
}