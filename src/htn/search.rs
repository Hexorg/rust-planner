use std::{collections::HashMap, cmp::Reverse};

use priority_queue::PriorityQueue;

// pub struct Astar {}

pub trait LinkedNode<'a, Node:'a, T> {
    fn neighbors(&'a self) -> T where T: Iterator<Item = (&'a Node, i32)>;
}

// impl Astar {
    fn reconstruct_path<'a, Node>(cameFrom:HashMap<&'a Node, &'a Node>, current:&'a Node) -> Vec<&'a Node> where Node:std::cmp::Eq + std::hash::Hash  {
        let mut total_path = Vec::new();
        let mut current = current;
        total_path.push(current);
        while cameFrom.contains_key(current) {
            current = cameFrom[current];
            total_path.push(current);
        }
        return total_path;
    }
    pub fn Astar<'a, Node:'a, F, I>(start:&'a Node, goal:&Node, heuristic: F) -> Option<Vec<&'a Node>> where I:Iterator<Item = (&'a Node, i32)>, Node:std::cmp::Eq + std::hash::Hash + LinkedNode<'a, Node, I>, F: Fn(&Node)->i32 {
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
                for (nbr, cost) in current.neighbors() {
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