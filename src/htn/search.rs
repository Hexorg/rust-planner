use std::rc::Rc;
use std::{collections::HashMap, cmp::Reverse, slice::Iter};
use super::domain::{Domain};
use super::parser::{Expr, Stmt};
use super::planner::{State, Planner};
use priority_queue::PriorityQueue;

#[derive(Clone)]
pub struct StateAndPath {
    pub state:State,
    pub method_name:Rc<String>
}

impl std::hash::Hash for StateAndPath {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.state.hash(state);
    }
}

impl std::cmp::PartialEq for StateAndPath {
    fn eq(&self, other: &Self) -> bool {
        self.state == other.state 
    }
}

impl std::cmp::Eq for StateAndPath { }


// impl Astar {
    fn reconstruct_path<'a>(came_from:HashMap<Rc<String>, Rc<String>>, current:Rc<String>) -> Vec<Rc<String>> where {
        let mut total_path = Vec::new();
        let mut current = current.clone();
        total_path.push(current.clone());
        while came_from.contains_key(&current) {
            current = came_from[&current].clone();
            total_path.push(current.clone());
        }
        
        return total_path.iter().rev().skip(1).map(|p| p.clone()).collect();
    }
    pub fn Astar<'a, F>(start:StateAndPath, goal:&'a Stmt, heuristic: F, domain:&'a Domain) -> Option<Vec<Rc<String>>> where  F: Fn(&StateAndPath)->i32 {
        let mut openSet = PriorityQueue::new();
        let mut cameFrom = HashMap::new();
        let mut gScore = HashMap::new();
        let mut fScore = HashMap::new();
        let mut currentCost = heuristic(&start);
        let goal_name = goal.name().unwrap();
        gScore.insert(goal_name.clone(), 0);
        fScore.insert(goal_name.clone(), currentCost);
        openSet.push(start.clone(), Reverse(currentCost));
        let mut is_first_run = false;
        let all_tasks:Vec<Rc<String>> = domain.tasks.keys().filter(|p| p.as_str() != start.method_name.as_str()).map(|k| k.clone()).collect();
        while openSet.len() > 0 {
            if let Some((current, _)) = openSet.pop() {
                if goal.are_preconditions_satisfied(&current.state.0).unwrap() == 1 {
                    // println!("goal preconditions are now satisfied with {:?}", current.state.0);
                    return Some(reconstruct_path(cameFrom, current.method_name));
                }
                // println!("Getting neighbors of {}", current.last_task_name);
                let neighbors = if is_first_run {
                    is_first_run = false;
                    domain.neighbors.get(&goal_name).unwrap()
                } else {
                    // domain.neighbors.get(&current.method_name).unwrap()
                    &all_tasks
                };
                // println!("Looking at state that resulted from calling {}: {:?}", current.method_name, current.state);
                for task_name in neighbors {
                    // print!("Can we run {}? ", task_name);
                    let cost = 5;
                    let task = domain.tasks.get(task_name).unwrap();
                    if task.are_preconditions_satisfied(&current.state.0).unwrap() == 1 {
                        // println!("conditions are satisfied");
                        let tentative_gScore = if let Some(score) = gScore.get(&current.method_name) {
                            score + cost
                        } else {
                            99999
                        };
                        let mut new_state = current.clone();
                        task.effect(&mut new_state.state);
                        new_state.method_name = task_name.clone();
                        if !gScore.contains_key(&new_state.method_name) || tentative_gScore < gScore[&new_state.method_name] {
                            cameFrom.insert(new_state.method_name.clone(), current.method_name.clone());
                            // println!("Adding hop from {} to {}", new_state.method_name.clone(), current.method_name.clone());
                            gScore.insert(new_state.method_name.clone(), tentative_gScore);
                            currentCost = tentative_gScore+heuristic(&new_state);
                            if !fScore.contains_key(&new_state.method_name) {
                                openSet.push(new_state.clone(), Reverse(currentCost));
                            }
                            fScore.insert(new_state.method_name, currentCost);
                        }
                    } else {
                        // println!("conditions are NOT satisfied");
                    }
                }
            } else {
                // Open set is empty but goal was never reached
                return None;
            }
        }
        return None;
    }
// }