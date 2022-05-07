use std::collections::HashMap;
use priority_queue::PriorityQueue;

use super::problem::Problem;
use super::domain::Domain;
use super::action::Action;

pub struct State {
    data: i32
}

pub struct Node<'a> {
    state: &'a State,
    parent: Option<&'a Node<'a>>,
    action: &'a Action<'a>,
    cost: f64,
    heuristic: f64,
    depth: i32
}

fn estimate_cost(state: &State, goal:&State) -> f64 {
    1.0
}

pub fn search<'a>(d: Domain, p: Problem) -> Option<Node<'a>> {
    let state = get_init_state();
    let close_set: HashMap<State, Node> = HashMap::new();
    let open_set: HashMap<State, Node> = HashMap::new();
    let open: PriorityQueue<Node> = PriorityQueue::new();

    let root = Node{state: &state, 
        parent: None, 
        action: -1, 
        cost: 0.0, 
        heuristic: estimate_cost(state, p.goal)
    };

    open.add(root);
    open_set.add(root);


    while open.len() > 0 {
        let current = open.poll();
        open_set.remove(current);
        close_set.put(current);

        if current.satisfy(p.goal) {
            return Some(current);
        } else {
            d.actions.iter().for_each(|action| {
                if action.is_applicable(current) {
                    let new_state = current.copy().apply(action);
                    let g = current.cost + action.cost;
                    if let Some(known_node) = open_set[new_state] {
                        if g < known_node.cost {
                            known_node.cost = g;
                            known_node.parent = Some(current);
                            known_node.action = action;
                            known_node.depth = current.depth + 1;
                        }
                    } else {
                        if let Some(known_node) = close_set[new_state] {
                            if g < known_node.cost {
                                known_node.cost = g;
                                known_node.parent = Some(current);
                                known_node.action = action;
                                known_node.depth = current.depth + 1;
                                open.add(known_node);
                                open_set.put(new_state, known_node);
                                close_set.remove(known_node);
                            }
                        } else {
                            let new_node = Node {
                                state: new_state,
                                cost: g,
                                parent: Some(current),
                                action: action,
                                heuristic: estimate_cost(new_state, p.goal),
                                depth: current.depth + 1,
                            };
                            open.add(new_state);
                            open_set.put(new_state, new_node);
                        }
                    }
                }
            });
        }
    }
    None
}