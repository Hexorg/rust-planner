use std::rc::Rc;
use std::{collections::HashMap, cmp::Reverse, slice::Iter};
use super::domain::{Domain};
use super::parser::{Expr, Stmt};
use super::planner::{State, Planner};
use priority_queue::PriorityQueue;

#[derive(Clone)]
struct StateAndPath {
    state:State,
    method_name:Rc<String>
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
        return total_path;
    }
    pub fn Astar<'a, F>(start:State, goal:&'a Stmt, heuristic: F, domain:&'a Domain) -> Option<Vec<Rc<String>>> where  F: Fn(&State)->i32 {
        let mut openSet = PriorityQueue::new();
        let mut cameFrom = HashMap::new();
        let mut gScore = HashMap::new();
        let mut fScore = HashMap::new();
        let mut currentCost = heuristic(&start);
        let goal_name = goal.name().unwrap();
        gScore.insert(goal_name.clone(), 0);
        fScore.insert(goal_name.clone(), currentCost);
        openSet.push(StateAndPath{state:start.clone(), method_name:goal_name.clone()}, Reverse(currentCost));
        let mut is_first_run = true;
        while openSet.len() > 0 {
            if let Some((current, _)) = openSet.pop() {
                if goal.are_preconditions_satisfied(&current.state.0).unwrap() == 1 {
                    return Some(reconstruct_path(cameFrom, current.method_name));
                }
                // println!("Getting neighbors of {}", current.last_task_name);
                let neighbors = if is_first_run {
                    is_first_run = false;
                    domain.neighbors.get(&goal_name).unwrap()
                } else {
                    domain.neighbors.get(&current.method_name).unwrap()
                };
                for task_name in neighbors {
                    let cost = 5;
                    let task = domain.tasks.get(task_name).unwrap();
                    let tentative_gScore = gScore[&current.method_name] + cost;
                    let mut new_state = current.clone();
                    task.effect(&mut new_state.state);
                    new_state.method_name = task_name.clone();
                    if !gScore.contains_key(&new_state.method_name) || tentative_gScore < gScore[&new_state.method_name] {
                        cameFrom.insert(new_state.method_name.clone(), current.method_name.clone());
                        gScore.insert(new_state.method_name.clone(), tentative_gScore);
                        currentCost = tentative_gScore+heuristic(&new_state.state);
                        if !fScore.contains_key(&new_state.method_name) {
                            openSet.push(new_state.clone(), Reverse(currentCost));
                        }
                        fScore.insert(new_state.method_name, currentCost);
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