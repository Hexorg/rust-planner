use std::{collections::HashMap, cmp::Reverse};
use super::domain::Domain;
use priority_queue::PriorityQueue;

// pub struct Astar {}

pub trait LinkedNode<'a, Node:'a, T> {
    fn neighbors(&'a self) -> T where T: Iterator<Item = &'a String>;
}

// impl Astar {
    fn reconstruct_path<'a>(cameFrom:HashMap<&'a String, &'a String>, current:&'a String) -> Vec<&'a String> where {
        let mut total_path = Vec::new();
        let mut current = current;
        total_path.push(current);
        while cameFrom.contains_key(current) {
            current = cameFrom[current];
            total_path.push(current);
        }
        return total_path;
    }
    pub fn Astar<'a, F>(start:&'a String, goal:&'a String, heuristic: F, domain:&'a Domain) -> Option<Vec<&'a String>> where  F: Fn(&String)->i32 {
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
                if current == goal {
                    return Some(reconstruct_path(cameFrom, current));
                }
                
                for nbr in &domain.tasks.get(current).unwrap().neighbors {
                    let cost = domain.cost(current, nbr);
                    let tentative_gScore = gScore[current] + cost;
                    if tentative_gScore < gScore[nbr] {
                        cameFrom.insert(nbr, current);
                        gScore.insert(nbr, tentative_gScore);
                        currentCost = tentative_gScore+heuristic(nbr);
                        if !fScore.contains_key(nbr) {
                            openSet.push(nbr, Reverse(currentCost));
                        }
                        fScore.insert(nbr, currentCost);
                        

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