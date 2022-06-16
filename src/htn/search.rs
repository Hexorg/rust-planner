use std::{collections::HashMap, cmp::Reverse};
use super::interpreter::{State, Evaluatable};

use super::parser::{Literal, Stmt};
use super::planner::{Planner, Error};
use priority_queue::PriorityQueue;

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct Node<'a, T:Copy> {
    pub state:State<T>,
    pub task_name:&'a str,
}


fn are_preconditions_satisfied<T>(stmt:&Stmt, state:&State<T>) -> Result<bool, Error> where T: Copy + 
    std::hash::Hash + 
    Default +
    std::cmp::PartialEq +
    std::cmp::PartialOrd +
    std::convert::From::<bool> +
    std::convert::Into::<bool> +
    std::convert::From::<Literal> +
    std::fmt::Display + 
    std::ops::Sub<Output = T> + 
    std::ops::Add<Output = T> + 
    std::ops::Div<Output = T> + 
    std::ops::Mul<Output = T> +
    std::ops::BitOr<Output = T> + 
    std::ops::BitAnd<Output = T> + 
    std::ops::Not<Output = T> {
        if let Some(p) = stmt.preconditions().expect("This statement can't have preconditions.") {
            match p.eval(state) {
                Ok(v) => Ok(v.into()),
                Err(e) => Err(e.into())
            }
        } else {
            Ok(true)
        }
}



// impl Astar {
    fn reconstruct_path<'a, T>(came_from:HashMap<Node<'a, T>, Node<'a, T>>, current:Node<'a, T>) -> Vec<String> where T:Copy + std::fmt::Debug + std::cmp::Eq + std::hash::Hash {
        let mut plan = Vec::<String>::new();
        // println!("Reconstructing path to {}... ", current.task_name);
        // println!("Came_from is {:?}", came_from);
        plan.push(String::from(current.task_name));
        let mut current = current;
        while came_from.contains_key(&current) {
            current = came_from[&current].clone();
            plan.push(String::from(current.task_name));
        }
        // println!();
        return plan.iter().rev().skip(1).map(|t| t.clone()).collect();
    }

    /// Outputs a plan UP TO goal, but not including goal
    pub fn Astar<'a, T, F>(start:State<T>, goal:&'a Stmt, heuristic: F, planner:&'a Planner) -> Result<Option<(Vec<String>, i32)>, Error> 
        where  F: Fn(&State<T>)->i32,
        T: Copy + Default + 
        std::hash::Hash + 
        std::cmp::PartialEq +
        std::cmp::Eq +
        std::cmp::PartialOrd +
        std::cmp::Ord + 
        std::fmt::Debug + 
        std::fmt::Display + 
        std::convert::Into::<i32> +
        std::convert::From::<bool> +
        std::convert::Into::<bool> +
        std::convert::From::<Literal> +
        std::ops::Sub<Output = T> + 
        std::ops::Add<Output = T> + 
        std::ops::Div<Output = T> + 
        std::ops::Mul<Output = T> +
        std::ops::BitOr<Output = T> + 
        std::ops::BitAnd<Output = T> + 
        std::ops::Not<Output = T>
        {
        let mut openSet = PriorityQueue::new();
        let mut cameFrom:HashMap<Node<T>, Node<T>> = HashMap::new();

        // For node n, gScore[n] is the cost of the cheapest path from start to n currently known.
        let mut gScore = HashMap::<State<T>, i32>::new();

        // For node n, fScore[n] := gScore[n] + h(n). fScore[n] represents our current best guess as to
        // how short a path from start to finish can be if it goes through n.
        let mut fScore = HashMap::<State<T>, i32>::new();
        // Down5
        // Up2
        // Down3
        // Up1
        // Down 4
        // up2
        // 
        let currentCost = heuristic(&start);
        openSet.push(Node{state:start.clone(), task_name:"__start__"}, Reverse(currentCost.clone()));
        gScore.insert(start.clone(),  0.into()); // Cost to reach N
        fScore.insert(start.clone(), currentCost); // Estimated total path cost if it goes through N

        // println!("Start state is {:?}", start);
        
        while let Some((current, total_plan_cost)) = openSet.pop() {
            // println!("Looking at state that resulted from calling {}: {:?}. Cost to reach: {}", current.task_name, current.state, total_plan_cost.0);
            if are_preconditions_satisfied(goal, &current.state).unwrap() {
                // println!("ASTAR Success: Can reach {} now with {:?} after {}", goal.name().unwrap(), current.state, current.task_name);
                // println!("came from:");
                // cameFrom.iter().for_each(|(k, v)| println!("From {} to {}", k, v.method_name));
                return Ok(Some((reconstruct_path(cameFrom, current), total_plan_cost.0)));
            }
            // println!("Getting neighbors of {}", current.method_name);
            let neighbors = planner.domain.get_enablers_of(current.task_name);
            for task_name in neighbors {
                // println!("Can we run {}? ", task_name);
                let (task, cost) = planner.get_task_and_cost(task_name, &current.state)?;
                if are_preconditions_satisfied(task, &current.state).unwrap() {
                    // println!("conditions are satisfied for {}", task_name);
                    // println!("Current state is {:?}", current.state);
                    // println!("gScore contains current.state? {}", gScore.contains_key(&current.state));
                    // println!("gScore.keys(): {:?}", gScore.keys());
                    let tentative_gScore = gScore.get(&current.state).unwrap_or(&i32::MAX).clone() + cost;
                    // println!("After {}({:?}) tentative_gScore is {}",current.task_name, current.state, tentative_gScore);
                    let mut new_state = current.state.clone();
                    if let Some(stmt) = task.effects()? {
                        for op in stmt.expressions()? {  
                            op.eval_mut(&mut new_state)?; 
                        } 
                    }
                    if !gScore.contains_key(&new_state) || tentative_gScore < gScore[&new_state] {
                        let new_node = Node{state:new_state.clone(), task_name};
                        // println!("Adding hop from {} to {}", task_name.clone(), current.task_name.clone());
                        cameFrom.insert(new_node.clone(), current.clone());
                        
                        gScore.insert(new_state.clone(), tentative_gScore.clone());
                        let currentCost = tentative_gScore + heuristic(&new_state);
                        if !fScore.contains_key(&new_state) {
                            // println!("New task {} gets us closer to the goal (score {})", task.name().unwrap(), tentative_gScore);
                            openSet.push(new_node, Reverse(currentCost));
                        }
                        fScore.insert(new_state, currentCost);
                    } else {
                        // println!("This task provides an existing state");
                    }
                } else {
                    // println!("conditions are NOT satisfied");
                }
            }
            // println!("No more neighbors");
            // println!("There are {} reachable nodes", openSet.len());
        }
        // println!("ASTAR FAIL");
        return Ok(None);
    }
// }