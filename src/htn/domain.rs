use std::{str::FromStr, ops::Deref, collections::HashSet};

use super::parser::{ParserError, Parser, Stmt, Expr};

pub struct Domain {
    ast: Vec<Stmt>,
}

impl FromStr for Domain {
    type Err = ParserError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let domain = Domain{ast:Parser::parse(s)?};
        let mut world_vars = HashSet::<&str>::new();
        let mut blackboard_vars = HashSet::<&str>::new();
        domain.ast.iter().for_each(|item| {
            world_vars.extend(Domain::get_world_variables_from_stmt(item).iter());
            blackboard_vars.extend(Domain::get_blackboard_variables_from_stmt(item).iter());
        });
        println!("world_vars: {:?}", world_vars);
        println!("blackboard_vars: {:?}", blackboard_vars);
        Ok(domain)
    }
}

impl Domain {
    fn get_world_variables_from_expr<'a>(expr: &'a Expr) -> HashSet<&'a str> {
        let mut r = HashSet::<&'a str>::new();
        match expr {
            Expr::Binary(left, _, right) => {r.extend(& mut Domain::get_world_variables_from_expr(left.deref()).iter()); r.extend(& mut Domain::get_world_variables_from_expr(right.deref()).iter())},
            Expr::Grouping(e) => r.extend(& mut Domain::get_world_variables_from_expr(e.deref()).iter()),
            Expr::Variable(var) => { r.insert(var.as_str()); } ,
            Expr::Unary(_, right) => r.extend(& mut Domain::get_world_variables_from_expr(right.deref()).iter()),
            Expr::Assignment(_, right) => r.extend(& mut Domain::get_world_variables_from_expr(right.deref()).iter()),
            Expr::Call(_, _, vexp) => vexp.iter().for_each(|item| r.extend(& mut Domain::get_world_variables_from_expr(item).iter())),
            _ => (),
        }
        r
    }
    fn get_world_variables_from_stmt<'a>(stmt: &'a Stmt) -> HashSet<&'a str> {
        let mut r = HashSet::<&'a str>::new();
        match stmt {
            Stmt::Method(_, Some(conditions), _) => r.extend(&mut Domain::get_world_variables_from_expr(conditions).iter()), 
            Stmt::Task(_, cnd, imp, _) => {if let Some(conditions) = cnd { r.extend(&mut Domain::get_world_variables_from_expr(conditions).iter()); } r.extend(&mut Domain::get_world_variables_from_stmt(imp).iter())},
            Stmt::Block(imp) => imp.iter().for_each(|item| r.extend(&mut Domain::get_world_variables_from_stmt(item).iter())),
            _ => (),
        }
        r
    }
    fn get_blackboard_variables_from_expr<'a>(expr: &'a Expr) -> HashSet<&'a str> {
        let mut r = HashSet::<&'a str>::new();
        match expr {
            Expr::Binary(left, _, right) => {r.extend(& mut Domain::get_blackboard_variables_from_expr(left.deref()).iter()); r.extend(& mut Domain::get_world_variables_from_expr(right.deref()).iter())},
            Expr::Grouping(e) => r.extend(& mut Domain::get_blackboard_variables_from_expr(e.deref()).iter()),
            Expr::Unary(_, right) => r.extend(& mut Domain::get_blackboard_variables_from_expr(right.deref()).iter()),
            Expr::Assignment(var, _) => {r.insert(var.as_str());},
            Expr::Call(_, _, vexp) => vexp.iter().for_each(|item| r.extend(& mut Domain::get_world_variables_from_expr(item).iter())),
            _ => (),
        }
        r
    }
    fn get_blackboard_variables_from_stmt<'a>(stmt: &'a Stmt) -> HashSet<&'a str> {
        let mut r = HashSet::<&'a str>::new();
        match stmt {
            Stmt::Task(_, _, imp, _) => { r.extend(&mut Domain::get_blackboard_variables_from_stmt(imp).iter())},
            Stmt::Block(imp) => imp.iter().for_each(|item| r.extend(&mut Domain::get_blackboard_variables_from_stmt(item).iter())),
            Stmt::Expression(e) => r.extend(Domain::get_blackboard_variables_from_expr(e).iter()),
            _ => (),
        }
        r
    }
}