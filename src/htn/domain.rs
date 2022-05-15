use std::{fs, fmt};

use super::parser::{Parser, Stmt, Expr};

pub struct Domain {
    pub ast: Vec<Stmt>,
    pub main_id: usize,
}

pub struct DomainError {
    message: String,
}

impl fmt::Debug for DomainError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

enum TaskType {
    COMPOSITE,
    PRIMITIVE
}

impl Domain {
    fn check_task_validity(stmt:&Stmt) -> Result<(),DomainError> {
        if let Stmt::Task(task_name, _, def, _) = stmt {
            let mut task_type:Option<TaskType> = None;
            match def.as_ref() {
                Stmt::Block(blk) =>
                    for substmt in blk {
                        match substmt {
                            Stmt::Method(_, _, _) => match task_type {
                                Some(TaskType::PRIMITIVE) => { return Err(DomainError{message:format!("Primitive Task {} can not have methods.", task_name)})},
                                None => {task_type = Some(TaskType::COMPOSITE);},
                                _ => ()
                            },
                            Stmt::Expression(e) => {
                                if let Expr::Call(_, _, _) | Expr::Assignment(_, _) = e {
                                    match task_type {
                                        Some(TaskType::COMPOSITE) => { return Err(DomainError{message:format!("Composite Task {} can not have operator calls.", task_name)})},
                                        None => {task_type = Some(TaskType::PRIMITIVE);},
                                        _ => ()
                                    }
                                } else {
                                    return Err(DomainError{message:format!("Primitive Task {} can only have operator calls.", task_name)})
                                }
                            }
                            _ => {return Err(DomainError{message:format!("Unexpected statement inside Task {}.", task_name)})},
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

    fn find_main(stmt:&Vec<Stmt>) -> Result<usize,DomainError> {
        for (i, s) in stmt.iter().enumerate() {
            if let Stmt::Task(name, _,_,_) = &s {
                match name.as_str() {
                    "main" => return Ok(i),
                    _ => (),
                }
            } 
        }
        Err(DomainError{message:String::from("Task 'main' not found.")})
    }

    pub fn from_file(filepath:&str) -> Result<Domain, DomainError> {
        let htn_source = fs::read_to_string(filepath).expect("File error:");
        let (ast, errors) = Parser::parse(htn_source.as_str());
        Parser::print_parse_errors(errors, htn_source.as_str(), filepath);
        Domain::rules_check(&ast)?;
        let main_id = Domain::find_main(&ast)?;
        Ok(Domain{ast, main_id})
    }
}