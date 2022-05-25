use std::{collections::HashMap, cmp::Reverse, slice::Iter};
use super::domain::{Domain, Task};
use super::parser::Expr;
use super::planner::{State, Planner};
use priority_queue::PriorityQueue;

// pub struct Astar {}

pub trait LinkedNode<'a> {
    fn preconditions(&'a self) -> &'a Option<Expr>;
    fn effects(&'a self) -> &'a Vec<Expr>;
    fn neighbors(&'a self) -> Iter<String>;
    fn cost(&'a self) -> i32;
}

// impl Astar {
    fn reconstruct_path<'a>(came_from:HashMap<String, String>, current:&String) -> Vec<String> where {
        let mut total_path = Vec::new();
        let mut current = current.clone();
        total_path.push(current.clone());
        while came_from.contains_key(&current) {
            current = came_from[&current].clone();
            total_path.push(current.clone());
        }
        return total_path;
    }
    pub fn Astar<'a, T, F>(start:State, goal:&'a T, heuristic: F, domain:&'a Domain) -> Option<Vec<String>> where T:LinkedNode<'a>, F: Fn(&State)->i32 {
        let mut openSet = PriorityQueue::new();
        let mut cameFrom = HashMap::new();
        let mut gScore = HashMap::new();
        let mut fScore = HashMap::new();
        let mut currentCost = heuristic(&start);
        gScore.insert(start.last_task_name.clone(), 0);
        fScore.insert(start.last_task_name.clone(), currentCost);
        openSet.push(start.clone(), Reverse(currentCost));
        let mut is_first_iter = true;
        while openSet.len() > 0 {
            if let Some((current, _)) = openSet.pop() {
                if goal.preconditions().as_ref().unwrap().eval(&current.s).expect("Unexpected precondition expression") == 1 {
                    return Some(reconstruct_path(cameFrom, &current.last_task_name));
                }

                // println!("Getting neighbors of {}", current.last_task_name);
                let task_map = if is_first_iter {
                    is_first_iter = false;
                    domain.tasks.get(&current.last_task_name).expect(format!("Unexpected task name {}", current.last_task_name).as_str()).neighbors().map(|nbr| domain.tasks.get(nbr)).collect()
                } else {
                    vec![goal].iter().map(|i| i).collect()
                };
                for task in task_map {
                    let tentative_gScore = gScore[&current.last_task_name] + task.cost();
                    let mut new_state = State{s:current.s.clone(), last_task_name:task.name.clone()};
                    task.effects().iter().for_each(|e| {e.eval_mut(&mut new_state.s);});
                    if !gScore.contains_key(&new_state.last_task_name) || tentative_gScore < gScore[&new_state.last_task_name] {
                        cameFrom.insert(new_state.last_task_name.clone(), current.last_task_name.clone());
                        gScore.insert(new_state.last_task_name.clone(), tentative_gScore);
                        currentCost = tentative_gScore+heuristic(&new_state);
                        if !fScore.contains_key(&new_state.last_task_name) {
                            openSet.push(new_state.clone(), Reverse(currentCost));
                        }
                        fScore.insert(new_state.last_task_name, currentCost);
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