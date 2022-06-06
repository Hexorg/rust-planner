use std::rc::Rc;
use std::{collections::HashMap, cmp::Reverse, slice::Iter};
use crate::htn::planner::PlannedTask;

use super::domain::{Domain};
use super::parser::{Expr, Stmt};
use super::planner::{State, Planner, Plan};
use priority_queue::PriorityQueue;

#[derive(Clone, Debug)]
pub struct StateAndPath {
    pub state:State,
    pub method_name:Rc<String>,
    pub cost: i32
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
    fn reconstruct_path<'a>(came_from:HashMap<Rc<String>, StateAndPath>, current:StateAndPath) -> Plan where {
        let mut plan = Plan::new();
        // print!("Reconstructing path... ");
        // print!("{}({}), ", current.method_name, current.cost);
        let mut planned_task = PlannedTask{name:current.method_name.clone(), cost:current.cost, end_state:current.state.clone(), operators:Vec::new(), preconditions:None};
        plan.push(planned_task.clone());
        let mut current = current;
        // print!("{}({}), ", current.method_name, current.cost);
        while came_from.contains_key(&current.method_name) {
            current = came_from[&current.method_name].clone();
            planned_task.name = current.method_name.clone();
            planned_task.cost = current.cost;
            plan.push(planned_task.clone());
            // print!("{}({}), ", current.method_name, current.cost);
        }
        // println!();
        return Plan(plan.0.iter().rev().skip(1).map(|t| t.clone()).collect());
    }
    pub fn Astar<'a, F>(start:StateAndPath, goal:&'a Stmt, heuristic: F, domain:&'a Domain) -> Option<Plan> where  F: Fn(&StateAndPath)->i32 {
        let mut openSet = PriorityQueue::new();
        let mut cameFrom:HashMap<Rc<String>, StateAndPath> = HashMap::new();

        // For node n, gScore[n] is the cost of the cheapest path from start to n currently known.
        let mut gScore = HashMap::new();

        // For node n, fScore[n] := gScore[n] + h(n). fScore[n] represents our current best guess as to
        // how short a path from start to finish can be if it goes through n.
        let mut fScore = HashMap::new();

        let mut currentCost = heuristic(&start);
        let goal_name = goal.name().unwrap();
        gScore.insert(start.method_name.clone(), 0);
        fScore.insert(start.method_name.clone(), currentCost);
        openSet.push(start.clone(), Reverse(currentCost));
        let mut is_first_run = false;
        let all_tasks:Vec<Rc<String>> = domain.get_all_task_names().iter().filter(|p| p.as_str() != start.method_name.as_str()).map(|k| k.clone()).collect();
        while let Some((current, _)) = openSet.pop() {
            // println!("Looking at state that resulted from calling {}: {:?}", current.method_name, current.state);
            if goal.are_preconditions_satisfied(&current.state.0).unwrap() == 1 {
                // println!("goal preconditions are now satisfied with {:?}", current.state.0);
                // println!("came from:");
                // cameFrom.iter().for_each(|(k, v)| println!("From {} to {}", k, v.method_name));
                return Some(reconstruct_path(cameFrom, current));
            }
            // println!("Getting neighbors of {}", current.method_name);
            let neighbors = &all_tasks;
            for task_name in neighbors {
                // println!("Can we run {}? ", task_name);
                let (task, cost) = domain.get_task_and_cost(task_name).unwrap();
                if task.are_preconditions_satisfied(&current.state.0).unwrap() == 1 {
                    // println!("conditions are satisfied");
                    let tentative_gScore = if let Some(score) = gScore.get(&current.method_name) {
                        score + cost
                    } else {
                        99999
                    };
                    let mut new_state = current.clone();
                    new_state.cost = tentative_gScore;
                    task.effect(&mut new_state.state);
                    new_state.method_name = task_name.clone();
                    if !gScore.contains_key(&new_state.method_name) || tentative_gScore < gScore[&new_state.method_name] {
                        cameFrom.insert(new_state.method_name.clone(), current.clone());
                        // println!("Adding hop from {} to {}", new_state.method_name.clone(), current.method_name.clone());
                        gScore.insert(new_state.method_name.clone(), tentative_gScore);
                        currentCost = tentative_gScore+heuristic(&new_state);
                        if !fScore.contains_key(&new_state.method_name) {
                            // println!("New task {} gets us closer to the goal", task.name().unwrap());
                            openSet.push(new_state.clone(), Reverse(currentCost));
                        }
                        fScore.insert(new_state.method_name, currentCost);
                    }
                } else {
                    // println!("conditions are NOT satisfied");
                }
            }
        }
        return None;
    }
// }