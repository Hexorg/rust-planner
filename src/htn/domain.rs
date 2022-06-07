use std::{fs, fmt::Debug,  collections::HashMap, rc::Rc, ops::Deref};
use super::parser::{Parser, Stmt, StmtFormatter};
use super::parser;

/// Structure that holds parsed out AST as well as optimization data
pub struct Domain {
    pub tasks: Vec<Stmt>,
    task_ids: HashMap<Rc<String>, usize>,
    main_idx: usize,
    pub filepath: String,
    pub content: String,
}

#[derive(Debug)]
pub enum Error {
    IO(std::io::Error),
    Parser(Vec<parser::Error>)
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IO(e) => write!(f, "{}", e),
            Self::Parser(e) => {for es in e { write!(f, "{}", es)?} Ok(())}
        }
        
    }
}


impl std::error::Error for Error { }

impl From<std::io::Error> for Error {
    fn from(arg: std::io::Error) -> Self {
        Self::IO(arg)
    }
}

impl From<parser::Error> for Error {
    fn from(arg: parser::Error) -> Self {
        Self::Parser(vec![arg])
    }
}

impl From<Vec<parser::Error>> for Error {
    fn from(arg: Vec<parser::Error>) -> Self {
        Self::Parser(arg)
    }
}

impl std::fmt::Display for Domain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Domain({}):\n", self.filepath)?;
    
        let mut max_line_count = 0;
        if let Some(mut largest_line) = self.tasks.iter().last().map(|task| task.line_no()) {
            while largest_line > 0 {
                largest_line /= 10;
                max_line_count += 1;
            }
        }
        self.tasks.iter().try_for_each(|stmt| write!(f, "{}", StmtFormatter{depth:0, max_line_count, stmt}))
    }
}

impl std::fmt::Debug for Domain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Domain({}):\n", self.filepath)?;
        let mut max_line_count = 0;
        if let Some(mut largest_line) = self.tasks.iter().last().map(|task| task.line_no()) {
            while largest_line > 0 {
                largest_line /= 10;
                max_line_count += 1;
            }
        }
        self.tasks.iter().try_for_each(|stmt| write!(f, "{:?}", StmtFormatter{depth:0, max_line_count, stmt}))
    }
}

impl Domain {
    // fn create_links(tasks:&Vec<Stmt>) -> HashMap<Rc<String>, Vec<Rc<String>>> {
    //     let mut result = HashMap::new();
    //     for task in tasks.iter() {
    //         let mut neighbors:Vec<Rc<String>> = Vec::new();
    //         for other in tasks.iter() {
    //             if task.name() != other.name() {
    //                 let affects = other.affects();
    //                 task.for_each_method(&mut |method| if affects.intersection(&method.depends()).count() > 0 {
    //                     println!("{} enables {}.{}", other_name, task_name, method.name().unwrap());
    //                     neighbors.push(method.name()?);
    //                     Ok(())
    //                 } else { Ok(())});
    //                 // if task effects other's preconditions
    //                 if affects.intersection(&task.depends()).count() > 0 {
    //                     neighbors.push(other_name.clone());
    //                     println!("{} enables {}", other_name, task_name);
    //                 }
    //             }
    //         }
    //         result.insert(task_name.clone(), neighbors);
    //         task.for_each_method(&mut |method| {
    //             let depends = method.depends();
    //             let mut neighbors:Vec<Rc<String>> = Vec::new();
    //             for (other_name, other) in tasks.iter() {
    //                 if other.affects().intersection(&depends).count() > 0 {
    //                     neighbors.push(other_name.clone())
    //                 }
    //             }
    //             result.insert(method.name().unwrap(), neighbors);
    //             Ok(())
    //         });
    //     }
    //     result
    // }

    pub fn print_parse_error(&self, error: &parser::Error) {
        Parser::print_parse_errors(error, &self.content, &self.filepath);
    }

    pub fn get_task(&self, name:&Rc<String>) -> Option<&Stmt> {
        if let Some(idx) = self.task_ids.get(name) {
            self.tasks.get(*idx)
        } else {
            None
        }
    }


    pub fn get_main(&self) -> &Stmt {
        self.tasks.get(self.main_idx).unwrap()
    } 


    pub fn get_all_task_names(&self) -> Vec<Rc<String>> {
        self.task_ids.keys().map(|key| key.clone()).collect()
    }

    pub fn from_file(filepath:&str) -> Result<Domain, Error> {
        let content = fs::read_to_string(filepath)?;
        let tasks = Parser::parse(content.as_str())?;
        // errors.iter().for_each(|e| Parser::print_parse_errors(e, content.as_str(), filepath));
        let mut task_ids = HashMap::new();
        let mut main_idx = None;
        for (idx, stmt) in tasks.iter().enumerate() {
            let name = stmt.name()?;
            if name.deref().eq("Main") {
                main_idx = Some(idx);
            }
            task_ids.insert(name.clone(), idx);
            
        }
    
        let domain = Domain{tasks,  task_ids, main_idx:main_idx.expect("Domain must contain 'Main' task"),  content, filepath:String::from(filepath)};
        Ok(domain)
    }
}