use std::{collections::HashMap, ops::Deref};

use crate::htn::search::Astar;

use super::{domain::Domain, parser::{Expr, TokenData}};

pub struct Planner<'a> {
    domain: &'a Domain,
    state: HashMap<String, i32>,
    last_task: String,
}

#[derive(Debug)]
enum ExpressionResult {
    Literal(i32), 
    Task(String),
    Variable(String),
    FunctionCall(String)
}

impl Planner<'_>{
    fn run_expr_i32(&mut self, expr:&Expr) -> i32 {
        match expr {
            Expr::Binary(left, op, right) => match op.t {
                TokenData::MINUS => self.run_expr_i32(left) - self.run_expr_i32(right),
                TokenData::PLUS => self.run_expr_i32(left) + self.run_expr_i32(right),
                TokenData::STAR => self.run_expr_i32(left) * self.run_expr_i32(right),
                TokenData::SLASH => self.run_expr_i32(left) / self.run_expr_i32(right),
                TokenData::GREATER => if self.run_expr_i32(left) > self.run_expr_i32(right) {1} else {0},
                TokenData::SMALLER => if self.run_expr_i32(left) < self.run_expr_i32(right) {1} else {0},
                TokenData::GREATER_OR_EQUALS => if self.run_expr_i32(left) >= self.run_expr_i32(right) {1} else {0},
                TokenData::SMALLER_OR_EQUALS => if self.run_expr_i32(left) <= self.run_expr_i32(right) {1} else {0},
                TokenData::NOT_EQUALS => if self.run_expr_i32(left) != self.run_expr_i32(right) {1} else {0},
                TokenData::OR => self.run_expr_i32(left) | self.run_expr_i32(right),
                TokenData::AND => self.run_expr_i32(left) & self.run_expr_i32(right),
                _ => 0,
            },
            Expr::Unary(op, left) => match op.t {
                TokenData::NOT => if self.run_expr_i32(left) == 0 {1} else {0},
                _ => 0,
            },
            Expr::Variable(_) => if let ExpressionResult::Literal(lit) = self.run_expr(expr) {
                lit
            } else {
                0
            },
            Expr::Literal(l) => *l,
            _ => panic!(format!("Unexpected expression {:?}", expr)),
        }
    }
    fn run_expr(&mut self, expr:&Expr) -> ExpressionResult {
        // println!("Running expression {:?}", expr);
        match expr {
            Expr::Binary(_,_,_) | Expr::Unary(_,_) => ExpressionResult::Literal(self.run_expr_i32(&expr)),
            Expr::Grouping(e) => self.run_expr(e.deref()),
            Expr::Literal(val) => ExpressionResult::Literal(*val),
            Expr::Variable(var) => {
                if let Some(val) = self.state.get(var.as_str()) {
                    ExpressionResult::Literal(*val)
                } else if self.domain.tasks.contains_key(var.as_str()) {
                    ExpressionResult::Task(var.clone())
                } else {
                    ExpressionResult::Variable(var.clone())
                }
            },
            Expr::Assignment(var, e) => {let val = self.run_expr(e.deref()); match &val {
                ExpressionResult::Literal(lit) => {self.state.insert(var.to_string(), *lit);},
                ExpressionResult::FunctionCall(func) => println!("\tStoring function call {} result to blackboard as {}", func, var),
                ExpressionResult::Variable(var) => panic!(format!("Unknown variable {}", var)),
                ExpressionResult::Task(_) => panic!("Tasks don't have results and can't be assigned"),
            } val},
            Expr::Call(target, _, args) => { let a = args.iter().map(|item| self.run_expr(item)); 
                let target = self.run_expr(target.deref());
                match target {
                    ExpressionResult::Literal(_) => panic!("Can't call a literal"),
                    ExpressionResult::Task(ref s) => {self.run_task(s); ExpressionResult::Literal(0)},
                    ExpressionResult::Variable(ref func) => {if self.domain.tasks.contains_key(func) { ExpressionResult::Task(func.clone())} else { println!("\tCalling function {}({:?})", func, args); ExpressionResult::FunctionCall(func.clone())}},
                    ExpressionResult::FunctionCall(func) => panic!(format!("Can't call result of a function call {}", func)),
                }
                },
            
        }
        
    }

    fn run_task(&mut self, task_name: &str) {
        let task = self.domain.tasks.get(task_name).unwrap();
        if let Some(ref preconditions) = task.preconditions {
            let r = self.run_expr(preconditions);
            match r {
                ExpressionResult::Literal(val) => if val == 0 { todo!("Figure out how to enable this task")},
                _ => {println!("{:?}", r); panic!("Unexpected precondition result.");}
            }
        }
        match &task.body {
            super::domain::TaskStatement::Composite(ref methods) => {
                let mut is_run_method = false;
                for method_name in methods {
                    println!("Method_name:{} ", method_name);
                    let method = self.domain.tasks.get(method_name).unwrap();
                    if if let Some(ref preconditions) = method.preconditions {
                        let r = self.run_expr(preconditions);
                        if let ExpressionResult::Literal(val) = r {
                            val == 1
                        } else {
                            println!("Expression result: {:?}", r);
                            panic!("Unexpected precondition result.");
                        }
                    } else {
                        true
                    } {
                        is_run_method = true;
                        self.run_task(method_name);
                    }
                }
                if !is_run_method {
                    for method_name in methods {
                        let path = Astar(&self.last_task, method_name, |f| 5, self.domain);
                        println!("Path: {:?}", path);
                    }
                    todo!("Figure out how to enable a method.");
                }
            },
            super::domain::TaskStatement::Primitive(body, _) => body.iter().for_each(|expr| {self.run_expr(expr);}),
        }
    }
    // fn run_main(&mut self) {
    //     self.run_stmt(self.ast.get(self.main_id).unwrap())
    // }
    pub fn run<'a>(domain: &'a Domain) {
        let mut state = HashMap::new();
        for var in &domain.world_variables {
            state.insert(var.clone(), 0);
        }
        state.insert(String::from("isHungry"), 1);
        let mut planner = Planner{domain, state, last_task:String::from("Main")};
        planner.run_task("Main");
    }
}