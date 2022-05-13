use std::{fmt, str::FromStr, ops::Deref, collections::HashSet};

use super::parser::{ParserError, Parser, Stmt, Expr};

pub struct Domain {
    ast: Vec<Stmt>,
}
pub enum DomainParsingError {
    Parser(ParserError),
    Domain(DomainError)
}

impl fmt::Debug for DomainParsingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parser(pe) => f.debug_tuple("Parser").field(arg0).finish(),
            Self::Domain(de) => eprintln!("Error: {}", de.message),
        }
    }
}

pub struct DomainError {
    message: String,
}

impl FromStr for Domain {
    type Err = DomainParsingError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match Parser::parse(s, None) {
            Ok(ast) => {Domain::rules_check(&ast); Ok(Domain{ast})},
            Err(e) => Err(DomainParsingError::Parser(e))
        }
    }
}

enum TaskType {
    COMPOSITE,
    PRIMITIVE
}

impl Domain {
    fn check_task_validity(stmt:&Stmt) -> Result<(),DomainError> {
        if let Stmt::Task(_, _, def, _) = stmt {
            let mut task_type:Option<TaskType> = None;
            match def.as_ref() {
                Stmt::Block(blk) =>
                    for substmt in blk {
                        match substmt {
                            Stmt::Method(_, _, _) => match task_type {
                                Some(TaskType::PRIMITIVE) => { return Err(DomainError{message:String::from("Task can either have methods or call operators.")})},
                                None => {task_type = Some(TaskType::COMPOSITE);},
                                _ => ()
                            },
                            Stmt::Expression(e) => {
                                if let Expr::Call(_, _, _) = e {
                                    match task_type {
                                        Some(TaskType::COMPOSITE) => { return Err(DomainError{message:String::from("Task can either have methods or call operators.")})},
                                        None => {task_type = Some(TaskType::PRIMITIVE);},
                                        _ => ()
                                    }
                                } else {
                                    return Err(DomainError{message:String::from("Task can either have methods or call operators.")})
                                }
                            }
                            _ => {return Err(DomainError{message:String::from("Task can either have methods or call operators.")})},
                        }
                    },
                _ => (),
            }
        }
        Ok(())
    }

    fn rules_check(stmt:&Vec<Stmt>) -> Result<(),DomainError> {
        for s in stmt {
            Domain::check_task_validity(s)?;    
        }
        Ok(())
    }
}