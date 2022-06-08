use std::{fmt, collections::HashMap, rc::Rc, cmp::Reverse, ops::Deref};


use priority_queue::PriorityQueue;
use super::interpreter::{State, Evaluatable};
use super::{domain::Domain, search::Astar, parser::{self, Literal, Stmt, Expr}};

pub struct Error {
    message:String,
}

impl std::error::Error for Error { }

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}
impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}
impl std::convert::From<parser::Error> for Error {
    fn from(e: parser::Error) -> Self {
        Error{message:format!("{}", e)}
    }
}

#[derive(Clone)]
pub struct PlannedTask<'a> {
    pub stmt:&'a Stmt,
    pub is_complete: bool,
}

impl std::fmt::Display for PlannedTask<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.stmt.name().unwrap())
    }
}

impl std::fmt::Debug for PlannedTask<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.stmt.name().unwrap())?;
        let none = Rc::new(String::from("None"));
        for op in self.stmt.expressions().unwrap() {
            write!(f, "{}, ", op.get_call_target().unwrap_or(none.clone()))?;
        }
        write!(f, ")")
    }
}

pub struct Plan<'a> (
     pub Vec<PlannedTask<'a>>
);

impl<'a> std::iter::IntoIterator for Plan<'a> { // So that you can say `for action in plan`
    type Item = PlannedTask<'a>;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}


impl<'a> Plan<'a> {
    #[inline]
    pub fn new() -> Self {
        Self(Vec::new())
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn push(&mut self, task: PlannedTask<'a>) {
        self.0.push(task)
    }

    // #[inline]
    // pub fn last_mut(&mut self) -> Option<&mut PlannedTask<'a>> {
    //     self.0.last_mut()
    // }

    #[inline]
    pub fn get(&self, index:usize) -> Option<&PlannedTask> {
        self.0.get(index)
    }

}

impl<'a> std::fmt::Display for Plan<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} task plan: ", self.len())?;
        let mut i = self.0.iter();
        i.by_ref().take(1).try_for_each(|task| write!(f, "{}", task))?;
        i.try_for_each(|task| write!(f, ", {}", task))
    }
}

impl<'a> std::fmt::Debug for Plan<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} task plan: ", self.0.len())?;
        //i.by_ref().take(1).try_for_each(|task| write!(f, "{:?}", task))?;
        self.0.iter().try_for_each(|task| write!(f, "{:?}", task))
    }
}
 

impl<'a> From<&'a Stmt> for PlannedTask<'a> {
    fn from(stmt: &'a Stmt) -> Self {
        Self{stmt, is_complete:true}
    }
}

impl<'a> PlannedTask<'a> {
    pub fn fail(stmt:&'a Stmt) -> Self {
        Self{stmt, is_complete:false}
    }
}


pub struct Planner<T> {
    pub domain:Domain,
    task_duration:HashMap<Rc<String>, T>,
}




impl<T> Planner<T> where T: Clone + 
    std::hash::Hash + 
    std::cmp::PartialEq +
    std::cmp::PartialOrd +
    std::cmp::Ord + 
    std::convert::From::<Literal> +
    std::fmt::Debug + 
    std::fmt::Display + 
    std::ops::Sub<Output = T> + 
    std::ops::Add<Output = T> + 
    std::ops::Div<Output = T> + 
    std::ops::Mul<Output = T> +
    std::ops::BitOr<Output = T> + 
    std::ops::BitAnd<Output = T> + 
    std::ops::Not<Output = T>{    
    pub fn get_cost(&self, stmt:&Stmt, state:&State<T>) -> Result<T, Error> {
        Ok(stmt.cost()?.and_then(|e| Some(e.eval(state).unwrap())).unwrap_or(Literal::F(0.0).into()))
    }

    pub fn get_task_and_cost(&self, task:&Rc<String>, state:&State<T>) -> Result<(&Stmt, T), Error> {
        if let Some(task) = self.domain.get_task(task) { 
            let cost = self.get_cost(task, state)?;
            Ok((task, cost))
        } else { 
            Err(Error{message:String::from("Task is not found.")})
        }
        
    }

    fn run_astar<'a>(&'a self, plan:&mut Plan<'a>, state:&mut State<T>, task: &'a Stmt) -> Result<bool, Error> {
        if plan.len() > 40 && task.name()? == self.domain.get_main().name()? {
            return Ok(true)
        }

        // println!("Figuring out plan for {}", task.name()?);

        if let Some((task_plan, _task_plan_cost)) = Astar(state.clone(), task, |f| Literal::F(4.0).into(), self)? {
            // println!("Astar returned cost{} {:?}", _task_plan_cost, task_plan);
            for subtask in task_plan {
                if !self.run_astar(plan, state, self.domain.get_task(&subtask).unwrap())? {
                    return Err(task.to_err(String::from("Planner thought task is achievable but it's not")).into());
                }
            }
            // Ready to run this task
            if task.is_composite()? {
                let mut method_plans = PriorityQueue::new();
                for method in task.methods()? {
                    if let Some((mut method_plan, method_plan_cost)) = Astar(state.clone(), method, |f| Literal::F(4.0).into(), self)? {
                        method_plan.push(method.name()?);
                        method_plans.push(method_plan, Reverse(method_plan_cost));
                    }
                }
                if let Some((mut method_plan, _method_plan_cost)) = method_plans.pop() { // Get the cheapest method to run
                    let method_name = method_plan.pop().unwrap();
                    for subtask in method_plan {
                        if !self.run_astar(plan, state, self.domain.get_task(&subtask).unwrap())? {
                            return Err(task.to_err(String::from("Planner thought task is achievable but it's not")).into());
                        }
                    }
                    // ready to run method
                    
                    for method in task.methods()? {
                        if method.name()? == method_name {
                            plan.push(PlannedTask::from(method));
                            // todo!("Iterate over operators here");
                            for op in method.expressions()? {
                                if let Some(call_target) = op.get_call_target() {
                                    if let Some(call_task) = self.domain.get_task(&call_target) {
                                        self.run_astar(plan, state, call_task)?;
                                    }
                                }
                            }
                            break
                        }    
                    }
                    
                } else {
                    // no methods are reachable
                    plan.push(PlannedTask::fail(task))
                }
            } else {
                // primitive task is reachable by definition
                plan.push(PlannedTask::from(task));
                for op in task.expressions()? {
                    if let Some(call_target) = op.get_call_target() {
                        if let Some(call_task) = self.domain.get_task(&call_target) {
                            self.run_astar(plan, state, call_task)?;
                        }
                    }
                }
            }
            if let Some(stmt) = task.effects()? {
                for op in stmt.expressions()? {  
                    op.eval_mut(state)?; 
                } 
            }
            Ok(plan.0.last().unwrap_or(&PlannedTask::from(task)).is_complete)
        } else {
            // this task isn't reachable
            Ok(false) 
        }
    }

    pub fn new(domain:Domain) -> Self {
        Planner{domain, task_duration:HashMap::new()}
    }

    pub fn plan<'a>(&'a self, state:&State<T>) -> Result<Plan<'a>, Error> {
        let mut plan = Plan::new();
        let mut state = state.clone();
        self.run_astar(&mut plan, &mut state, self.domain.get_main())?;
        Ok(plan)
    }
}