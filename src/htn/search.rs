use std::{collections::HashMap, cmp::Reverse};
use super::domain::{Operation, OperandType};
use super::vm::State;

use super::planner::Planner;
use priority_queue::PriorityQueue;

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq)]
pub struct Node {
    pub state:State,
    pub task_id:usize,
}

impl Node {
    #[inline]
    pub fn new(state:&State, id:usize) -> Self {
        Self{state:state.clone(), task_id:id}
    }
}


// impl Astar {
    fn reconstruct_path(came_from:HashMap<Node, Node>, current:&Node) -> Vec<usize> {
        let mut plan = Vec::<usize>::new();
        // println!("Reconstructing path to {}... ", current.task_name);
        // println!("Came_from is {:?}", came_from);
        plan.push(current.task_id);
        let mut current = current;
        while came_from.contains_key(&current) {
            current = &came_from[&current];
            plan.push(current.task_id);
        }
        // println!();
        return plan.iter().rev().skip(1).map(|idx| *idx).collect();
    }

    /// Outputs a plan UP TO goal, but not including goal
    pub fn Astar<F>(start:Node, goal:&Vec<Operation>, heuristic: F, planner:&Planner) -> Option<(Vec<usize>, i32)>
        where  F: Fn(&Node)->i32 {
        let mut openSet = PriorityQueue::new();
        let mut cameFrom:HashMap<Node, Node> = HashMap::new();

        // For node n, gScore[n] is the cost of the cheapest path from start to n currently known.
        let mut gScore = HashMap::<State, i32>::new();

        // For node n, fScore[n] := gScore[n] + h(n). fScore[n] represents our current best guess as to
        // how short a path from start to finish can be if it goes through n.
        let mut fScore = HashMap::<State, i32>::new();

        let currentCost = heuristic(&start);
        gScore.insert(start.state.clone(),  0.into()); // Cost to reach N
        fScore.insert(start.state.clone(), currentCost); // Estimated total path cost if it goes through N
        openSet.push(start, Reverse(currentCost.clone()));
        // println!("Start state is {:?}", start);
        
        while let Some((mut current, total_plan_cost)) = openSet.pop() {
            
            if current.state.eval(goal).unwrap().is_true() {
                return Some((reconstruct_path(cameFrom, &current), total_plan_cost.0));
            }
            let all_tasks = (0..planner.domain.tasks.len()).filter(|i| *i != planner.domain.get_main_id()).collect();
            let neighbors = planner.domain.neighbors.get(&current.task_id).unwrap_or(&all_tasks);
            for task_id in neighbors {
                // println!("Can we run {}? ", task_name);
                let (task, cost) = planner.get_task_and_cost(*task_id);
                if current.state.eval(task.preconditions()).unwrap().is_true() {
                    let tentative_gScore = gScore.get(&current.state).unwrap_or(&i32::MAX).clone() + cost;
                    let mut new_state = current.state.clone();
                    new_state.eval(task.effects());
                    if !gScore.contains_key(&new_state) || tentative_gScore < gScore[&new_state] {
                        let new_node = Node::new(&new_state, *task_id);
                        let new_cost = tentative_gScore + heuristic(&new_node);
                        // println!("Adding hop from {} to {}", task_name.clone(), current.task_name.clone());
                        cameFrom.insert(new_node.clone(), current.clone());
                        if !fScore.contains_key(&new_state) {
                            // println!("New task {} gets us closer to the goal (score {})", task.name().unwrap(), tentative_gScore);
                            openSet.push(new_node, Reverse(new_cost));
                        }
                        gScore.insert(new_state.clone(), tentative_gScore);
                        fScore.insert(new_state, new_cost);
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
        println!("ASTAR FAIL");
        return None;
    }
// }