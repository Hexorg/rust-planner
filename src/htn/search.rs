use std::{collections::HashMap, cmp::Reverse};

use super::{planner::{Planner, Statistics}, compiler::{state::State, Operation}};
use priority_queue::PriorityQueue;

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq)]
pub struct Node<'a> {
    pub state:State<'a>,
    pub task_id:usize,
}

impl<'a> Node<'a> {
    #[inline]
    pub fn new(state:&State<'a>, id:usize) -> Self {
        Self{state:state.clone(), task_id:id}
    }
}

fn reconstruct_path<'a>(came_from:HashMap<Node<'a>, Node<'a>>, current:&Node<'a>) -> Vec<usize> {
    let mut plan = Vec::<usize>::new();
    plan.push(current.task_id);
    let mut current = current;
    while came_from.contains_key(&current) {
        current = &came_from[&current];
        plan.push(current.task_id);
    }
    return plan.iter().rev().skip(1).map(|idx| *idx).collect(); 
}

/// Outputs a plan UP TO goal, but not including goal
pub fn a_star<F>(start:Node, goal:&[Operation], heuristic: F, planner:&Planner, statistics:&mut Statistics) -> Option<(Vec<usize>, i32)>
    where  F: Fn(&Node)->i32 {
    statistics.calls_to_astar += 1;
    let mut open_set = PriorityQueue::new();
    let mut came_from:HashMap<Node, Node> = HashMap::new();

    // AKA gScore
    let mut cheapest_known_cost_to_state = HashMap::<State, i32>::new();

    // AKA fScore
    let mut estimated_cost_to_goal_through_state = HashMap::<State, i32>::new();

    let estimated_cost_to_goal = heuristic(&start);
    // println!("Start state is {:?}", start.state);
    cheapest_known_cost_to_state.insert(start.state.clone(),  0.into()); // Cost to reach N
    estimated_cost_to_goal_through_state.insert(start.state.clone(), estimated_cost_to_goal); // Estimated total path cost if it goes through N
    open_set.push(start, Reverse(estimated_cost_to_goal));

    // let mut task_names : Vec<String> = planner.domain.task_mapping().iter().map(|(k,v)| k.clone()).collect();
    // task_names.sort_by(|l,r| planner.domain.task_mapping()[l].cmp(&planner.domain.task_mapping()[r]));
    
    while let Some((mut current, total_plan_cost)) = open_set.pop() {
        statistics.astar_visited_nodes += 1;
        statistics.calls_to_eval += 1;
        if current.state.eval(goal).unwrap().is_true() {
            return Some((reconstruct_path(came_from, &current), total_plan_cost.0));
        }
        let all_tasks = (0..planner.domain.tasks().len()).filter(|i| *i != planner.domain.main_id()).collect();
        let neighbors = planner.domain.neighbors().get(&current.task_id).unwrap_or(&all_tasks);
        for task_id in neighbors {
            // println!("Can we run {}? ", task_name);
            let (task, cost) = planner.get_task_and_cost(&current.state, *task_id);
            statistics.calls_to_eval += 1;
            if current.state.eval(&task.preconditions).unwrap().is_true() {
                let cost_to_neighboring_task = cheapest_known_cost_to_state.get(&current.state).unwrap_or(&i32::MAX).clone() + cost;
                // println!("Exploring running task {}: {}", task_id, task_names[*task_id]);
                let mut new_state = current.state.clone();
                statistics.calls_to_eval += 1;
                new_state.eval_mut(&task.effects);
                if !cheapest_known_cost_to_state.contains_key(&new_state) || cost_to_neighboring_task < cheapest_known_cost_to_state[&new_state] {
                    let new_node = Node::new(&new_state, *task_id);
                    let new_cost = cost_to_neighboring_task + heuristic(&new_node);
                    // println!("Adding hop from {} to {}", task_id, current.task_id);
                    came_from.insert(new_node.clone(), current.clone());
                    if !estimated_cost_to_goal_through_state.contains_key(&new_state) {
                        open_set.push(new_node, Reverse(new_cost));
                    }
                    cheapest_known_cost_to_state.insert(new_state.clone(), cost_to_neighboring_task);
                    estimated_cost_to_goal_through_state.insert(new_state, new_cost);
                }
            }
        }
    }
    return None;
}
