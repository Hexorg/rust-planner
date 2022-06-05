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

impl std::fmt::Display for PlanStep {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.operator)
    }
}

pub struct PlannedTask {
    pub preconditions: Option<Rc<Expr>>,
    pub name: Rc<String>,
    pub operators: Vec<PlanStep>,
    pub end_state: State,
}

impl std::fmt::Display for PlannedTask {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.name)?;
        let mut i = self.operators.iter();
        i.by_ref().take(1).try_for_each(|op| write!(f, "{}", op))?;
        i.try_for_each(|op| write!(f, ", {}", op))?;
        write!(f, ")")
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
}

impl std::fmt::Display for Plan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut i = self.0.iter();
        i.by_ref().take(1).try_for_each(|task| write!(f, "{}", task))?;
        i.try_for_each(|task| write!(f, ", {}", task))
    }
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

pub struct Planner {
    pub plan:Plan,
    last_successfull_task:Option<Rc<String>>,
    method_heatmap:HashMap<Rc<String>, i32>,
}


impl Planner{

    fn plan_to_run_task(&mut self, state:&mut State, task:&Stmt, domain:&Domain) -> Result<bool, Error> {
        let start = StateAndPath{state:state.clone(), method_name:self.last_successfull_task.as_ref().unwrap().clone()};
        if let Some(plan) = Astar(start, task, |f| 4, domain) {
            if plan.len() > 0 {
                for subtask in plan {
                    if !self.run_task(state, domain.tasks.get(&subtask).unwrap(), domain)? {
                        return Err(task.to_err(String::from("Planner thought task is achievable but it's not")));
                    }
                }
                self.run_task(state, task, domain)
            } else {
                Ok(false)
            }
        } else {
            Ok(false)
        }
    }
 

    fn run_task(&mut self, state:&mut State, task: &Stmt, domain:&Domain) -> Result<bool, Error> {
        if self.plan.len() > 40 && task.name()? == domain.main {
            return Ok(true)
        }
        if task.are_preconditions_satisfied(&state.0)? == 1 {
            if let Stmt::Task{name,..} = task {
                self.last_successfull_task = Some(name.clone());
                
            }
            
            // println!("Running task {}", task.name()?);
            let run_result = if task.is_composite()? {
                let mut queue = PriorityQueue::new();
                task.for_each_method(&mut |method| {
                    let method_name = method.name()?;
                    if method.are_preconditions_satisfied(&state.0)? == 1 {
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
                    task.for_each_method_while(&mut |method| Ok(!self.plan_to_run_task(state, method, domain)?))
                } else {
                    let mut is_success = false;
                    while let Some((method_name, _)) = queue.pop() {
                        task.for_each_method_while(&mut |method| if method.name()? == method_name { 
                            self.method_heatmap.insert(method_name.clone(), self.method_heatmap.get(&method_name).or(Some(&0)).unwrap()+1);
                            is_success = self.run_task(state, method, domain)?;
                            Ok(!is_success)
                        } else { 
                            Ok(true) 
                        })?;
                        if is_success {
                            break
                        }
                    }
                    Ok(is_success)
                }
                // println!("Done running composite task {}", task.name()?);
            } else {
                let name = match task {
                    Stmt::Task{name,..} => name.clone(),
                    Stmt::Method{name,..} => Rc::new(format!("{}.{}", self.last_successfull_task.as_ref().unwrap(), name)),
                    _ => task.name()?,
                };
                self.plan.push(PlannedTask{name:name.clone(), preconditions:task.preconditions()?.to_owned(), operators:Vec::new(), end_state:State(HashMap::new())});
                task.for_each_operator(&mut |op| {
                    if let Expr::Noop(_) = op { Ok(true) } else {
                        let target = op.get_call_target().expect("Only call expressions are supported in task/method bodies.");
                        if let Some(task) = domain.tasks.get(&target) {
                            if !self.run_task(state, task, domain)? {
                                let can_plan = self.plan_to_run_task(state, task, domain)?;
                                if !can_plan {
                                    // Tasks preconditions are unmet. whatever is the unment precondition is the plan-state requirement now
                                    self.plan.last_mut().unwrap().end_state = state.clone();
                                }
                                Ok(can_plan)
                            } else {
                                Ok(true)
                            }
                        } else {
                            // let op_type = if op.get_assignment_target().is_some() { "blackboard"} else { "simple" };
                            // println!("Calling {} operator {}", op_type, op);
                            self.plan.last_mut().unwrap().operators.push(PlanStep::from(op));
                            Ok(true)
                        }
                    }
                })
                // println!("Done running primitive task {}", task.name()?);
            };
            if let Ok(true) = run_result {
                task.effect(state);
                task.effect(&mut self.plan.last_mut().unwrap().end_state);
            }
            run_result
        } else {
            Ok(false)
        }
    }
    // fn run_main(&mut self) {
    //     self.run_stmt(self.ast.get(self.main_id).unwrap())
    // }
    pub fn new() -> Self {
        Planner{plan:Plan::new(), method_heatmap:HashMap::new(), last_successfull_task:None}
    }

    pub fn plan(&mut self, state:&State, domain: &Domain) -> Result<bool, Error>{
        self.plan = Plan::new();
        let mut state = state.clone();
        self.run_task(&mut state, domain.tasks.get(&domain.main).expect("Unable to find Main task"), domain)
    }
}