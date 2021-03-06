use std::{fmt, collections::HashMap, cmp::Reverse};


use priority_queue::PriorityQueue;
use super::{domain::Domain, compiler::{state::State, Operation, OperandType, Task, TaskBody}, search::{Node, a_star}};

#[derive(Debug)]
pub struct Error(String);

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for Error { }


#[derive(Debug)]
pub struct Plan (
     pub Vec<Operation>
);

impl<'a> std::iter::IntoIterator for Plan { // So that you can say `for action in plan`
    type Item = Operation;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug)]
pub struct Statistics {
    pub astar_visited_nodes:usize,
    pub calls_to_astar:usize,
    pub calls_to_eval:usize,
}

impl Statistics {
    #[inline]
    pub fn new() -> Self {
        Self { 
            astar_visited_nodes: 0, 
            calls_to_astar: 0,
            calls_to_eval: 0,
        }
    }
}


pub struct Planner {
    pub domain:Domain,
    task_duration:HashMap<usize, f32>,
}




impl Planner {
    pub fn new_state(&self) -> State { 
        State::new(self.domain.state_mapping())
    }
    #[allow(dead_code)]
    pub fn set_cost(&mut self, task_id:usize, cost:f32) {
        self.task_duration.insert(task_id, cost);
    }
    pub fn get_task_and_cost(&self, state: &State, task_id:usize) -> (&Task, i32) {
        let task = &self.domain.tasks()[task_id];
        let cost = if let OperandType::I(v) = state.eval(&task.cost).unwrap() { v } else { 0 };
        // println!("{} cost is {:?}", stmt.name()?, stmt.cost()?);
        let time = self.task_duration.get(&task_id).unwrap_or(&1.0);
        let r = cost + (10.0*time) as i32;
        (task, r)
    }

    fn add_primitive_task_operators<F>(&self, plan:&mut Plan, state:&mut State, stats:&mut Statistics, body:&Vec<Operation>, on_plan:&mut F) -> Result<bool, Error> 
    where F: FnMut(&Vec<Operation>, &mut State)->() {
        let mut all = true;
        for op in body {
            all &= match op {
                Operation::ReadBlackboard(_) |
                Operation::WriteBlackboard(_) |
                Operation::CallOperator(_, _) => {plan.0.push(*op); Ok(true)},
                Operation::PlanTask(task_id) => self.run_astar(plan, state, stats, *task_id, on_plan),
                _ => Err(Error(format!("Unexpected primitive task body operation: {:?}.", op))),
            }?;
            if !all {
                break;
            }
        }
        Ok(all)
    }


    fn run_astar<F>(&self, plan:&mut Plan, state:&mut State, stats:&mut Statistics, task_id:usize, on_plan:&mut F) -> Result<bool, Error> 
    where F: FnMut(&Vec<Operation>, &mut State)->() { 
        if plan.0.len() > 40 && task_id == self.domain.main_id() {
            return Ok(true)
        }
        let task = &self.domain.tasks()[task_id];
        let mut goal_state = state.clone();
        goal_state.admit(task.wants());
        let heuristic = |node:&Node| node.state.manhattan_distance(&goal_state);
        on_plan(&task.planning, state);
        if let Some((task_plan, _task_plan_cost)) = a_star(Node::new(state, usize::MAX), &task.preconditions, heuristic, self, stats) {
            for subtask in task_plan {
                self.run_astar(plan, state, stats, subtask, on_plan)?;
            }
            
            match &task.body {
                TaskBody::Primitive(ops) => {let r = self.add_primitive_task_operators(plan, state, stats, ops, on_plan);
                    state.eval_mut(&task.effects);
                    r},
                TaskBody::Composite(methods) => {
                    let mut method_plans = PriorityQueue::new();
                    for method_id in methods {
                        let method = &self.domain.tasks()[*method_id];
                        on_plan(&method.planning, state);
                        if let Some((mut method_plan, method_plan_cost)) = a_star(Node::new(state, usize::MAX), &method.preconditions, heuristic, self, stats) {
                            method_plan.push(*method_id);
                            let cost = if let OperandType::I(v) = state.eval(&method.cost).unwrap() { v } else { 0 };
                            method_plans.push(method_plan, Reverse(method_plan_cost+cost));
                        }
                    }
                    // println!("Method plans: {:?}", method_plans);
                    let mut result = Ok(false);
                    while let Some((method_plan, _method_plan_cost)) = method_plans.pop() { // Get the cheapest method to run
                        let mut all = true;
                        for subtask in method_plan {
                            all &= self.run_astar(plan, state, stats, subtask, on_plan)?;
                        }
                        if all {
                            result = Ok(true);
                            break
                        }
                    }
                    result
                }
            }
        } else {
            Ok(false)
        }
    }

    pub fn new(domain:Domain) -> Self {
        Planner{domain, task_duration:HashMap::new()}
    }

    pub fn plan<F>(&self, state:&State, on_plan:&mut F) -> Result<Plan, Error> 
    where F: FnMut(&Vec<Operation>, &mut State)->() {
        let mut plan = Plan(Vec::new());
        let mut state = state.clone();
        let mut stats = Statistics::new();
        println!("Running planning...");
        self.run_astar(&mut plan, &mut state, &mut stats, self.domain.main_id(), on_plan)?;
        println!("*** Statistics:\n{:?}", stats);
        Ok(plan)
    }
}