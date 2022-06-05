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

#[derive(Debug, Clone)]
pub struct PlanStep {
    pub assignment: Option<Rc<String>>,
    pub operator: Rc<String>,
    pub arguments:Vec<Rc<String>>
}

impl std::fmt::Display for PlanStep {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.operator)
    }
}

#[derive(Clone)]
pub struct PlannedTask {
    pub preconditions: Option<Rc<Expr>>,
    pub name: Rc<String>,
    pub cost: i32,
    pub operators: Vec<PlanStep>,
    pub end_state: State,
}

impl std::hash::Hash for PlannedTask {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.cost.hash(state);
    }
}

impl std::cmp::PartialEq for PlannedTask {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && 
        self.cost == other.cost
    }
}

impl std::cmp::Eq for PlannedTask { }


impl std::fmt::Display for PlannedTask {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.name)?;
        let mut i = self.operators.iter();
        i.by_ref().take(1).try_for_each(|op| write!(f, "{}", op))?;
        i.try_for_each(|op| write!(f, ", {}", op))?;
        write!(f, ")")
    }
}

impl From<&Stmt> for PlannedTask {
    fn from(_: &Stmt) -> Self {
        todo!()
    }
}

pub struct Plan (
     Vec<PlannedTask>
);

impl std::iter::IntoIterator for Plan { // So that you can say `for action in plan`
    type Item = PlannedTask;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}


impl Plan {
    #[inline]
    pub fn new() -> Self {
        Self(Vec::new())
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn push(&mut self, task: PlannedTask) {
        self.0.push(task)
    }

    #[inline]
    pub fn last_mut(&mut self) -> Option<&mut PlannedTask> {
        self.0.last_mut()
    }

    #[inline]
    pub fn get(&self, index:usize) -> Option<&PlannedTask> {
        self.0.get(index)
    }

    pub fn total_cost(&self) -> i32 {
        let mut cost = 0;
        for task in self.0.iter() {
            cost += task.cost;
        }
        cost
    }
}

impl std::fmt::Display for Plan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut i = self.0.iter();
        i.by_ref().take(1).try_for_each(|task| write!(f, "{}", task))?;
        i.try_for_each(|task| write!(f, ", {}", task))
    }
}

impl std::hash::Hash for Plan {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.iter().for_each(|i| i.hash(state))
    }
}

impl std::cmp::PartialEq for Plan {
    fn eq(&self, other: &Self) -> bool {
        match self.0.iter().zip(other.0.iter()).try_for_each(|(l, r)| if l == r { Ok(())} else {Err(())}) {
            Ok(()) => true,
            Err(()) => false
        }
    }
}

impl std::cmp::Eq for Plan { }
 

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

pub struct Planner {
    pub plan:Plan,
    last_successfull_task:Option<Rc<String>>,
    method_heatmap:HashMap<Rc<String>, i32>,
    entry_name: Rc<String>,
}


impl Planner{

    // fn plan_to_run_task(&mut self, state:&mut State, task:&Stmt, domain:&Domain) -> Result<bool, Error> {
    //     let start = StateAndPath{state:state.clone(), cost:0, method_name:self.last_successfull_task.as_ref().unwrap().clone()};
    //     if let Some(plan) = Astar(start, task, |f| 4, domain) {
    //         if plan.len() > 0 {
    //             for subtask in plan {
    //                 if !self.run_task(state, domain.get_task(&subtask).unwrap(), domain)? {
    //                     return Err(task.to_err(String::from("Planner thought task is achievable but it's not")));
    //                 }
    //             }
    //             self.run_task(state, task, domain)
    //         } else {
    //             Ok(false)
    //         }
    //     } else {
    //         Ok(false)
    //     }
    // }
 

    // fn run_task(&mut self, state:&mut State, task: &Stmt, domain:&Domain) -> Result<bool, Error> {
    //     if self.plan.len() > 40 && task.name()? == domain.get_main().name()? {
    //         return Ok(true)
    //     }
    //     if task.are_preconditions_satisfied(&state.0)? == 1 {
    //         if let Stmt::Task{name,..} = task {
    //             self.last_successfull_task = Some(name.clone());
                
    //         }
            
    //         // println!("Running task {}", task.name()?);
    //         let run_result = if task.is_composite()? {
    //             let mut queue = PriorityQueue::new();
    //             task.for_each_method(&mut |method| {
    //                 let method_name = method.name()?;
    //                 if method.are_preconditions_satisfied(&state.0)? == 1 {
    //                     queue.push(
    //                         method_name.clone(), 
    //                         Reverse(*self.method_heatmap.get(&method_name).or(Some(&0)).unwrap())
    //                     );
    //                 } else {
    //                     self.method_heatmap.insert(method.name().unwrap().clone(), -10);
    //                 }
    //                 Ok(())
    //             })?;
    //             if queue.len() == 0 { // no methods are statically satisfied
    //                 task.for_each_method_while(&mut |method| Ok(!self.plan_to_run_task(state, method, domain)?))
    //             } else {
    //                 let mut is_success = false;
    //                 while let Some((method_name, _)) = queue.pop() {
    //                     task.for_each_method_while(&mut |method| if method.name()? == method_name { 
    //                         self.method_heatmap.insert(method_name.clone(), self.method_heatmap.get(&method_name).or(Some(&0)).unwrap()+1);
    //                         is_success = self.run_task(state, method, domain)?;
    //                         Ok(!is_success)
    //                     } else { 
    //                         Ok(true) 
    //                     })?;
    //                     if is_success {
    //                         break
    //                     }
    //                 }
    //                 Ok(is_success)
    //             }
    //             // println!("Done running composite task {}", task.name()?);
    //         } else {
    //             let name = match task {
    //                 Stmt::Task{name,..} => name.clone(),
    //                 Stmt::Method{name,..} => Rc::new(format!("{}.{}", self.last_successfull_task.as_ref().unwrap(), name)),
    //                 _ => task.name()?,
    //             };
    //             self.plan.push(PlannedTask{name:name.clone(), preconditions:task.preconditions()?.to_owned(), operators:Vec::new(), end_state:State(HashMap::new())});
    //             task.for_each_operator(&mut |op| {
    //                 if let Expr::Noop(_) = op { Ok(true) } else {
    //                     let target = op.get_call_target().expect("Only call expressions are supported in task/method bodies.");
    //                     if let Some(task) = domain.get_task(&target) {
    //                         if !self.run_task(state, task, domain)? {
    //                             let can_plan = self.plan_to_run_task(state, task, domain)?;
    //                             if !can_plan {
    //                                 // Tasks preconditions are unmet. whatever is the unment precondition is the plan-state requirement now
    //                                 self.plan.last_mut().unwrap().end_state = state.clone();
    //                             }
    //                             Ok(can_plan)
    //                         } else {
    //                             Ok(true)
    //                         }
    //                     } else {
    //                         // let op_type = if op.get_assignment_target().is_some() { "blackboard"} else { "simple" };
    //                         // println!("Calling {} operator {}", op_type, op);
    //                         self.plan.last_mut().unwrap().operators.push(PlanStep::from(op));
    //                         Ok(true)
    //                     }
    //                 }
    //             })
    //             // println!("Done running primitive task {}", task.name()?);
    //         };
    //         if let Ok(true) = run_result {
    //             task.effect(state);
    //             task.effect(&mut self.plan.last_mut().unwrap().end_state);
    //         }
    //         run_result
    //     } else {
    //         Ok(false)
    //     }
    // }
    
    fn run_astar_only(&mut self, state:&mut State, task: &Stmt, domain:&Domain) -> Result<bool, Error> {
        if self.plan.len() > 40 && task.name()? == domain.get_main().name()? {
            return Ok(true)
        }
        let last_task = if let Some(ref method_name) = self.last_successfull_task {
            method_name.clone()
        } else {
            self.entry_name.clone()
        };
        let start = StateAndPath{state:state.clone(), cost:0, method_name:last_task};
        if let Some(plan) = Astar(start.clone(), task, |f| 4, domain) {
            for subtask in plan {
                if !self.run_astar_only(state, domain.get_task(&subtask.name).unwrap(), domain)? {
                    return Err(task.to_err(String::from("Planner thought task is achievable but it's not")));
                }
            }
            // Ready to run this task
            println!("Running task {}", task.name().unwrap());
            if task.is_composite()? {
                let mut method_plans = PriorityQueue::new();
                task.for_each_method(&mut |method| {
                    if let Some(mut mp) = Astar(start.clone(), method, |f| 10, domain){
                        let mp_cost = mp.total_cost();
                        let method_name = method.name()?;
                        mp.push(PlannedTask{name:method_name.clone(), cost:*domain.get_cost(&method_name).unwrap_or(&9999), end_state:State(HashMap::new()), operators:Vec::new(), preconditions:None});
                        method_plans.push(mp, mp_cost);
                    }
                    Ok(())
                })?;
                if let Some((plan, cost)) = method_plans.pop() {
                    // Cheapest cost is to run this method
                    println!("Best method is {}", plan.0.last().unwrap());
                    let mut plan_step = plan.0.last().unwrap().clone();
                    plan_step.name = Rc::new(format!("{}.{}", task.name()?.as_str(), plan_step.name));
                    self.plan.push(plan_step);
                    for subtask in plan {
                        if !self.run_astar_only(state, domain.get_task(&subtask.name).unwrap(), domain)? {
                            return Err(task.to_err(String::from("Planner thought task is achievable but it's not")));
                        }
                    }
                    Ok(true)
                } else {
                    // no methods are reachable
                    Ok(false) 
                }
            } else {
                self.plan.push(task)
                task.for_each_operator(&mut |op| {
                    if let Some(subtask) = domain.get_task(&op.get_call_target().unwrap()) {
                        self.run_astar_only(state, subtask, domain)
                    } else {
                        // call to an operator
                        self.plan.last_mut().unwrap().operators.push(PlanStep::from(op));
                        Ok(true)
                    }
                    
                })
            }
        } else {
            // this task isn't reachable
            Ok(false) 
        }
    }

    pub fn new() -> Self {
        Planner{plan:Plan::new(), method_heatmap:HashMap::new(), last_successfull_task:None, entry_name: Rc::new(String::from("__entry__"))}
    }

    pub fn plan(&mut self, state:&State, domain: &Domain) -> Result<bool, Error>{
        self.plan = Plan::new();
        let mut state = state.clone();
        self.run_astar_only(&mut state, domain.get_main(), domain)
    }
}