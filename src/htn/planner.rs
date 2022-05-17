use std::{slice::SliceIndex, collections::HashMap, rc::Rc, ops::Deref};

use super::{domain::Domain, parser::{Stmt, Expr, TokenData}};

pub struct Planner<'a> {
    // ast: &'a Vec<Stmt>,
    main_id: usize,
    state: HashMap<String, i32>,
    tasks: HashMap<String, &'a Stmt>,
}

enum ExpressionResult {
    Literal(i32), 
    Task(String),
    Variable(String),
    FunctionCall(String)
}

impl Planner<'_>{

    fn run_task(&mut self, name:&str, conditions:&Option<Expr>, definition:&Stmt, effects:Option<&Rc<Stmt>>) {
        println!("Running task {}", name);
        if let Some(cnd) = conditions {
            if let ExpressionResult::Literal(1) = self.run_expr(&cnd) {
                // println!("Task {} meeds conditions", name);
                self.run_stmt(definition);
                if let Some(eff) = effects {
                    self.run_stmt(eff.deref());
                }
            }
        } else {
            // println!("Task {} has no conditions", name);
            self.run_stmt(definition);
            if let Some(eff) = effects {
                self.run_stmt(eff.deref());
            }
        }
        
    }
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
                } else if let Some(_) = self.tasks.get(var.as_str()) {
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
                    ExpressionResult::Task(s) => {self.run_stmt(self.tasks.get(&s).unwrap()); ExpressionResult::Literal(0)},
                    ExpressionResult::Variable(func) => {println!("\tCalling function {}({:?})", func, args); ExpressionResult::FunctionCall(func.clone())},
                    ExpressionResult::FunctionCall(func) => panic!(format!("Can't call result of a function call {}", func)),
                }
                },
            
        }
        
    }
    fn run_stmt(&mut self, stmt:&Stmt) {
        // println!("Running statement {:?}", stmt);
        match stmt {
            Stmt::Method(_,_,_) => panic!("Methods can't be run directly"),
            Stmt::Task(name, conditions, definition, effects) => self.run_task(name, conditions, definition.deref(), effects.as_ref()),
            Stmt::Block(stmt_list) => {
                let mut  sat_methods = Vec::< Rc<Stmt> >::new();
                if stmt_list.iter().all(|item| if let Stmt::Method(_,cnd,def) = item { if cnd.is_some() { if let ExpressionResult::Literal(1) = self.run_expr(cnd.as_ref().unwrap()) { sat_methods.push(def.clone()); }} else {sat_methods.push(def.clone());} true } else { false}) {
                    // if here, we need to choose one method to run
                    self.run_stmt(sat_methods.get(0).unwrap().deref()); // TODO: Choose a random method
                } else {
                    // if here we need to run the whole block
                    for s in stmt_list {
                        self.run_stmt(s);
                    }
                }
            }
            Stmt::Expression(expr) => {self.run_expr(expr);},
        }

    }
    // fn run_main(&mut self) {
    //     self.run_stmt(self.ast.get(self.main_id).unwrap())
    // }
    pub fn run<'a>(domain: &'a Domain) {
        let mut tasks = HashMap::new();
        domain.ast.iter().for_each(|item| if let Stmt::Task(name, _,_,_) = item {tasks.insert(name.to_string(), item);});
        let mut state = HashMap::new();
        state.insert(String::from("WsCanSeeEnemy"), 0);
        let mut planner = Planner{main_id: domain.main_id, state, tasks};
        planner.run_stmt(domain.ast.get(planner.main_id).unwrap());
    }
}