use std::{collections::HashMap, cmp::Reverse};
use super::domain::{Domain, Task};
use super::parser::Expr;
use super::planner::{State, Planner};
use priority_queue::PriorityQueue;

// pub struct Astar {}

pub trait LinkedNode<'a, Node:'a, T> {
    fn neighbors(&'a self) -> T where T: Iterator<Item = &'a String>;
}

// impl Astar {
    fn reconstruct_path<'a>(came_from:HashMap<&'a State, &'a State>, current:&'a State) -> Vec<&'a State> where {
        let mut total_path = Vec::new();
        let mut current = current;
        total_path.push(current);
        while came_from.contains_key(current) {
            current = came_from[current];
            total_path.push(current);
        }
        return total_path;
    }
    pub fn Astar<'a, F>(start:&'a State, goal:&'a Task, heuristic: F, domain:&'a Domain) -> Option<Vec<&'a State>> where  F: Fn(&State)->i32 {
        let mut openSet = PriorityQueue::new();
        let mut cameFrom = HashMap::new();
        let mut gScore = HashMap::new();
        let mut fScore = HashMap::new();
        let mut currentCost = heuristic(start);
        openSet.push(start, Reverse(currentCost));
        gScore.insert(start, 0);
        fScore.insert(start, currentCost);
        while openSet.len() > 0 {
            if let Some((current, _)) = openSet.pop() {
                if goal.preconditions.as_ref().unwrap().eval(&start.0) == 1 {
                    return Some(reconstruct_path(cameFrom, current));
                }
                
                for (nbr, cost) in goal.neighbors(domain) {
                    let tentative_gScore = gScore[current] + cost;
                    let mut new_state = current.clone();
                    nbr.effects.iter().for_each(|e| {e.eval(&new_state.0);});
                    if tentative_gScore < gScore[new_state] {
                        cameFrom.insert(new_state, current);
                        gScore.insert(new_state, tentative_gScore);
                        currentCost = tentative_gScore+heuristic(new_state);
                        if !fScore.contains_key(new_state) {
                            openSet.push(new_state, Reverse(currentCost));
                        }
                        fScore.insert(new_state, currentCost);
                        

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