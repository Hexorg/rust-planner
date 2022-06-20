use std::time::Duration;
use std::{fmt, collections::HashMap, rc::Rc, cmp::Reverse, ops::Deref};


use priority_queue::PriorityQueue;
use super::{domain::{Domain, Operation, Task, ComplexTask, PrimitiveTask}, search::{Node, Astar}, vm::State};

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

    #[inline]
    pub fn reset(&mut self) {
        self.astar_visited_nodes = 0;
        self.calls_to_astar = 0;
    }
}


pub struct Planner {
    pub domain:Domain,
    task_duration:HashMap<usize, f32>,
}




impl Planner {
    pub fn new_state(&self) -> State { 
        State::new(self.domain.get_state_mapping().len())
    }
    pub fn set_cost(&mut self, task_id:usize, cost:f32) {
        self.task_duration.insert(task_id, cost);
    }
    pub fn get_task_and_cost(&self, task_id:usize) -> (&Task, i32) {

        let cost =  match self.domain.tasks[task_id] {
            Task::Complex(ComplexTask{cost,..})|
            Task::Primitive(PrimitiveTask{cost,..}) => cost,
        };
        // println!("{} cost is {:?}", stmt.name()?, stmt.cost()?);
        let time = self.task_duration.get(&task_id).unwrap_or(&1.0);
        let r = cost + (10.0*time) as i32;
        (&self.domain.tasks[task_id], r)
    }

    fn add_primitive_task_operators(&self, plan:&mut Plan, state:&mut State, stats:&mut Statistics, body:&Vec<Operation>) -> Result<bool, Error> {
        let mut all = true;
        for op in body {
            all &= match op {
                Operation::ReadBlackboard(_) |
                Operation::WriteBlackboard(_) |
                Operation::CallOperator(_, _) => {plan.0.push(*op); Ok(true)},
                Operation::PlanTask(task_id) => self.run_astar(plan, state, stats, *task_id),
                _ => Err(Error("Unexpected primitive task body operation.".to_owned())),
            }?;
        }
        Ok(all)
    }

    fn run_astar(&self, plan:&mut Plan, state:&mut State, stats:&mut Statistics, task_id:usize) -> Result<bool, Error> {
        if plan.0.len() > 40 && task_id == self.domain.get_main_id() {
            return Ok(true)
        }

        match self.domain.tasks.get(task_id).unwrap() {
            Task::Complex(ComplexTask { preconditions, cost, body, effects,.. }) => {
                if let Some((task_plan, _task_plan_cost)) = Astar(Node::new(state, usize::MAX), preconditions, |f| 4, self, stats) {
                    for subtask in task_plan {
                        self.run_astar(plan, state, stats, subtask)?;
                    }

                    let mut method_plans = PriorityQueue::new();
                    for (method_id, method) in body.iter().enumerate() {
                        if let Some((mut method_plan, method_plan_cost)) = Astar(Node::new(state, usize::MAX), &method.preconditions, |f| 4, self, stats) {
                            method_plan.push(method_id);
                            method_plans.push(method_plan, Reverse(method_plan_cost+cost));
                        }
                    }
                    // println!("Method plans: {:?}", method_plans);
                    if let Some((mut method_plan, _method_plan_cost)) = method_plans.pop() { // Get the cheapest method to run
                        let method_id = method_plan.pop().unwrap();
                        for subtask in method_plan {
                            self.run_astar(plan, state, stats, subtask)?;
                        }
                        let r = self.add_primitive_task_operators(plan, state, stats, &body[method_id].body);
                        state.eval(effects);
                        r
                    } else {
                        // No reachable methods
                        Ok(false)
                    }
                } else {
                    Ok(false)
                }
            },
            Task::Primitive(PrimitiveTask { preconditions, cost, body, effects,..}) => {
                if let Some((task_plan, _task_plan_cost)) = Astar(Node::new(state, usize::MAX), preconditions, |f| 4, self, stats) {
                    for subtask in task_plan {
                        self.run_astar(plan, state, stats, subtask)?;
                    }
                    let r = self.add_primitive_task_operators(plan, state, stats, body);
                    state.eval(effects);
                    r
                } else {
                    Ok(false)
                }
            },
        }
    }

    pub fn new(domain:Domain) -> Self {
        Planner{domain, task_duration:HashMap::new()}
    }

    pub fn plan(&self, state:&State) -> Result<Plan, Error> {
        let mut plan = Plan(Vec::new());
        let mut state = state.clone();
        let mut stats = Statistics::new();
        println!("Running planning...");
        self.run_astar(&mut plan, &mut state, &mut stats, self.domain.get_main_id())?;
        println!("*** Statistics:\n{:?}", stats);
        Ok(plan)
    }
}