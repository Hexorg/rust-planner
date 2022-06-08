use std::rc::Rc;
use std::{collections::HashMap, cmp::Reverse, slice::Iter};
use super::interpreter::{State, Evaluatable};

use super::parser::{self, Literal, Stmt};
use super::planner::{Planner, Error};
use priority_queue::PriorityQueue;

#[derive(Clone, Debug)]
pub struct Node<T: std::hash::Hash> {
    pub state:State<T>,
    pub task_name:Rc<String>,
}

impl<T: std::hash::Hash> std::hash::Hash for Node<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.state.hash(state);
    }
}

impl<T: std::hash::Hash + std::cmp::PartialEq + std::fmt::Debug> std::cmp::PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        self.state == other.state 
    }
}
impl<T: std::hash::Hash + std::cmp::PartialEq + std::fmt::Debug> std::cmp::Eq for Node<T> { }

impl<T:std::hash::Hash> Node<T> {
    fn new(state:State<T>, task_name:Rc<String>) -> Self {
        Self{state, task_name}
    }
}

fn are_preconditions_satisfied<T>(stmt:&Stmt, state:&State<T>) -> Result<T, Error> where T: Clone + 
    std::hash::Hash + 
    std::cmp::PartialEq +
    std::cmp::PartialOrd +
    std::convert::From::<Literal> +
    std::ops::Sub<Output = T> + 
    std::ops::Add<Output = T> + 
    std::ops::Div<Output = T> + 
    std::ops::Mul<Output = T> +
    std::ops::BitOr<Output = T> + 
    std::ops::BitAnd<Output = T> + 
    std::ops::Not<Output = T> {
        if let Some(p) = stmt.preconditions().expect("This statement can't have preconditions.") {
            match p.eval(state) {
                Ok(v) => Ok(v),
                Err(e) => Err(e.into())
            }
        } else {
            Ok(Literal::B(true).into())
        }
}



// impl Astar {
    fn reconstruct_path<'a>(came_from:HashMap<Rc<String>, Rc<String>>, current:Rc<String>) -> Vec<Rc<String>> where {
        let mut plan = Vec::new();
        // print!("Reconstructing path... ");
        // print!("{}({}), ", current.method_name, current.cost);
        plan.push(current.clone());
        let mut current = current;
        // print!("{}({}), ", current.method_name, current.cost);
        while came_from.contains_key(&current) {
            current = came_from[&current].clone();
            plan.push(current.clone());
            // print!("{}({}), ", current.method_name, current.cost);
        }
        // println!();
        return plan.iter().rev().skip(1).map(|t| t.clone()).collect();
    }

    /// Outputs a plan UP TO goal, but not including goal
    pub fn Astar<'a, T, F>(start:State<T>, goal:&'a Stmt, heuristic: F, planner:&Planner<T>) -> Result<Option<(Vec<Rc<String>>, T)>, Error> where  F: Fn(&State<T>)->T,
        T: Clone + 
        std::hash::Hash + 
        std::cmp::PartialEq +
        std::cmp::PartialOrd +
        std::cmp::Ord + 
        std::fmt::Debug + 
        std::fmt::Display + 
        std::convert::From::<Literal> +
        std::ops::Sub<Output = T> + 
        std::ops::Add<Output = T> + 
        std::ops::Div<Output = T> + 
        std::ops::Mul<Output = T> +
        std::ops::BitOr<Output = T> + 
        std::ops::BitAnd<Output = T> + 
        std::ops::Not<Output = T> {
        let mut openSet = PriorityQueue::new();
        let mut cameFrom:HashMap<Rc<String>, Rc<String>> = HashMap::new();

        // For node n, gScore[n] is the cost of the cheapest path from start to n currently known.
        let mut gScore = HashMap::<State<T>, T>::new();

        // For node n, fScore[n] := gScore[n] + h(n). fScore[n] represents our current best guess as to
        // how short a path from start to finish can be if it goes through n.
        let mut fScore = HashMap::<State<T>, T>::new();
        
        let currentCost = heuristic(&start);
        openSet.push(Node::new(start.clone(), Rc::new(String::from("__start__"))), Reverse(currentCost.clone()));
        gScore.insert(start.clone(),  Literal::F(0.0).into());
        fScore.insert(start.clone(), currentCost);

        // println!("Start state is {:?}", start);
        
        let all_tasks:Vec<Rc<String>> = planner.domain.get_all_task_names().iter().filter(|p| p.as_str() != "Main").map(|k| k.clone()).collect();
        while let Some((current, total_plan_cost)) = openSet.pop() {
            // println!("Looking at state that resulted from calling {}: {:?}. Cost to reach: {}", current.task_name, current.state, total_plan_cost.0);
            if are_preconditions_satisfied(goal, &current.state).unwrap() == Literal::B(true).into() {
                // println!("Can reach {} now with {:?}", goal.name().unwrap(), current.state.0);
                // println!("came from:");
                // cameFrom.iter().for_each(|(k, v)| println!("From {} to {}", k, v.method_name));
                return Ok(Some((reconstruct_path(cameFrom, current.task_name), total_plan_cost.0)));
            }
            // println!("Getting neighbors of {}", current.method_name);
            let neighbors = &all_tasks;
            for task_name in neighbors {
                // println!("Can we run {}? ", task_name);
                let (task, cost) = planner.get_task_and_cost(task_name, &current.state)?;
                if are_preconditions_satisfied(task, &current.state).unwrap() == Literal::B(true).into() {
                    // println!("conditions are satisfied");
                    // println!("Current state is {:?}", current.state);
                    // println!("gScore contains current.state? {}", gScore.contains_key(&current.state));
                    // println!("gScore.keys(): {:?}", gScore.keys());
                    let tentative_gScore = gScore.get(&current.state).unwrap_or(&Literal::F(f32::INFINITY).into()).clone();
                    // println!("After {}({:?}) tentative_gScore is {}",current.task_name, current.state, tentative_gScore);
                    let mut new_state = current.state.clone();
                    task.effects()?.as_ref().and_then(|s| {for op in s.expressions().unwrap() { op.eval_mut(&mut new_state); } Option::<i32>::None});
                    if !gScore.contains_key(&new_state) || tentative_gScore < gScore[&new_state] {
                        cameFrom.insert(task_name.clone(), current.task_name.clone());
                        // println!("Adding hop from {} to {}", new_state.method_name.clone(), current.method_name.clone());
                        gScore.insert(new_state.clone(), tentative_gScore.clone());
                        let currentCost = tentative_gScore + heuristic(&new_state);
                        if !fScore.contains_key(&new_state) {
                            // println!("New task {} gets us closer to the goal", task.name().unwrap());
                            openSet.push(Node::new(new_state.clone(), task_name.clone()), Reverse(currentCost.clone()));
                        }
                        fScore.insert(new_state, currentCost);
                    }
                } else {
                    // println!("conditions are NOT satisfied");
                }
            }
        }
        return Ok(None);
    }
// }