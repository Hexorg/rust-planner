use std::{slice::SliceIndex, collections::HashMap, rc::Rc, ops::Deref};

use super::{domain::Domain, parser::{Stmt, Expr, TokenData}};

pub struct Planner<'a> {
    ast: &'a Vec<Stmt>,
    main_id: usize,
    state: HashMap<String, i32>,
    tasks: HashMap<String, Stmt>,
}

enum ExpressionResult<'a> {
    Literal(i32), 
    Task(&'a Stmt),
    FunctionCall(&'a String)
}

impl Planner<'_>{

    fn run_task(&mut self, name:String, conditions:Option<Expr>, definition:Stmt, effects:Option<Rc<Stmt>>) {
        if let Some(cnd) = conditions {
            if let ExpressionResult::Literal(1) = self.run_expr(cnd) {
                println!("Running task {}", name);
                self.run_stmt(definition);
                if let Some(eff) = effects {
                    self.run_stmt(*eff.as_ref());
                }
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
            _ => panic!(),
        }
    }
    fn run_expr<'b>(&'b mut self, expr:Expr) -> ExpressionResult<'b> {
        match expr {
            Expr::Binary(_,_,_) | Expr::Unary(_,_) => ExpressionResult::Literal(self.run_expr_i32(&expr)),
            Expr::Grouping(e) => self.run_expr(*e.as_ref()),
            Expr::Literal(val) => ExpressionResult::Literal(val),
            Expr::Variable(var) => {
                if let Some(val) = self.state.get(var.as_str()) {
                    ExpressionResult::Literal(*val)
                } else if let Some(task) = self.tasks.get(var.as_str()) {
                    ExpressionResult::Task(task)
                } else {
                    ExpressionResult::FunctionCall(&var)
                }
            },
            Expr::Assignment(var, e) => {let val = self.run_expr(*e.as_ref()); match val {
                ExpressionResult::Literal(lit) => {self.state.insert(var.to_string(), lit);},
                ExpressionResult::FunctionCall(func) => println!("Storing function call {} result to blackboard as {}", func, var),
                ExpressionResult::Task(_) => panic!("Tasks don't have results and can't be assigned"),
            } val},
            Expr::Call(target, _, args) => { let a = args.iter().map(|item| self.run_expr(*item)); 
                let target = self.run_expr(*target.as_ref());
                match target {
                    ExpressionResult::Literal(_) => panic!("Can't call a literal"),
                    ExpressionResult::Task(s) => self.run_stmt(*s),
                    ExpressionResult::FunctionCall(func) => println!("Calling function {}", func),
                }
                ExpressionResult::Literal(0)},
            
        }
        
    }
    fn run_stmt(&mut self, stmt:Stmt) {
        match stmt {
            Stmt::Method(_,_,_) => panic!("Methods can't be run directly"),
            Stmt::Task(name, conditions, definition, effects) => self.run_task(name, conditions, *definition.as_ref(), effects),
            Stmt::Block(stmt_list) => {
                let mut  sat_methods = Vec::< Rc<Stmt> >::new();
                if stmt_list.iter().all(|item| if let Stmt::Method(_,cnd,def) = item { if cnd.is_some() { if let ExpressionResult::Literal(1) = self.run_expr(cnd.unwrap()) { sat_methods.push(*def); }} true } else { false}) {
                    // if here, we need to choose one method to run
                    self.run_stmt(*sat_methods.get(0).unwrap().as_ref()); // TODO: Choose a random method
                }
            }
            Stmt::Expression(expr) => {self.run_expr(expr);},
        }

    }
    fn run_main(&mut self) {
        self.run_stmt(*self.ast.get(self.main_id).unwrap())
    }
    pub fn run<'a>(domain: &'a Domain) {
        let mut tasks = HashMap::new();
        domain.ast.iter().for_each(|item| if let Stmt::Task(name, _,_,_) = item {tasks.insert(name.to_string(), *item);});
        let mut planner = Planner{ast: &domain.ast, main_id: domain.main_id, state:HashMap::new(), tasks};
        planner.run_main();
    }
}