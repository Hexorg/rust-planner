use std::{collections::HashMap, rc::Rc};


use super::{domain::Domain, search::Astar, parser::{Stmt, Expr, ParserError}};

#[derive(Debug, Clone)]
pub struct State(pub HashMap<Rc<String>, i32>);

impl std::hash::Hash for State {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.values().for_each(|val| val.hash(state))
    }
}
impl std::cmp::PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        let mut result = false;
        self.0.values().zip(other.0.values()).fold(&mut result, |acc, (left, right)| {*acc &=  left == right; acc});
        result
    }
}
impl std::cmp::Eq for State {

}
pub struct Planner<'a> {
    domain: &'a Domain,
    state: State,
    blackboard: HashMap<String, i32>,
    plan: Vec<String>
}

#[derive(Debug)]
enum ExpressionResult {
    Literal(i32), 
    Task(String),
    Variable(String),
    FunctionCall(String)
}

impl Planner<'_>{


 

    fn run_task(&mut self, task: &Stmt) -> Result<(), ParserError> {
        if task.are_preconditions_satisfied(&self.state.0)? == 1 {
            println!("Running task {}", task.name()?);
            if task.is_composite()? {
                let mut is_method_run = false;
                task.for_each_method_while(&mut |method| if method.are_preconditions_satisfied(&self.state.0).unwrap() == 1 {
                    self.run_task(method);
                    is_method_run = true;
                    false } else { true
                });
                if !is_method_run { // no methods are statically satisfied
                    let mut path = Vec::new();
                    task.for_each_method_while(&mut |method| { // figure out which method can be reached through search
                        if let Some(plan) = Astar(self.state.clone(), method, |f| 4, self.domain) {
                            path = plan;
                            false
                        } else {
                            true
                        }
                    });
                    if path.len() > 0 {
                        println!("Path: {:?}", path);
                        todo!("Run path")
                    }
                }
                if !is_method_run {
                    panic!("No solutions found to reach {} methods", task.name()?);
                }
                println!("Done running composite task {}", task.name()?);
            } else {
                task.for_each_operator(&mut |op| {
                    if let Some(target) = op.get_assignment_target() {
                        println!("Storing next call to blackboard as {}", target);
                    }
                    let target = op.get_call_target().expect("Only call expressions are supported in task/method bodies.");
                    if let Some(task) = self.domain.tasks.get(&target) {
                        self.run_task(task);
                    } else {
                        println!("Calling operator {}", target);
                    }
                });
                println!("Done running primitive task {}", task.name()?);
            }
            task.effect(&mut self.state);
        }
        Ok(())
    }
    // fn run_main(&mut self) {
    //     self.run_stmt(self.ast.get(self.main_id).unwrap())
    // }
    pub fn run<'a>(domain: &'a Domain) -> Result<Vec<String>, ParserError> {
        let mut state = State(HashMap::new());
        for task in domain.tasks.values() {
            for var in task.affects().iter().chain(task.depends().iter()) {
                state.0.insert(var.clone(), 0);
            }

        }
        state.0.insert(Rc::new(String::from("WsCanSeeEnemy")), 1);
        let mut planner = Planner{domain, state, blackboard:HashMap::new(), plan:Vec::new()};
        planner.run_task(domain.tasks.get(&domain.main).expect("Unable to find Main task"));
        Ok(planner.plan)
    }
}