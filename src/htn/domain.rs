use std::{fs, fmt::Debug,  collections::HashMap, rc::Rc, ops::Deref};
use super::parser::{Parser, Stmt, StmtFormatter};
use super::parser;

pub struct Domain {
    pub tasks: HashMap<Rc<String>, Stmt>,
    pub neighbors: HashMap<Rc<String>, Vec<Rc<String>>>,
    pub main: Rc<String>,
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
    
        let mut sorted = self.tasks.values().collect::<Vec<&Stmt>>();
        let mut largest_line = 0;
        sorted.sort_by(|a, b| {
            let a_line = a.line_no();
            let b_line = b.line_no();
            let r = a_line.cmp(&b_line); 
            largest_line = match r {
                std::cmp::Ordering::Less => b_line,
                std::cmp::Ordering::Equal |
                std::cmp::Ordering::Greater => a_line,
            };
            r
        });
        let mut max_line_count = 0;
        while largest_line > 0 {
            largest_line /= 10;
            max_line_count += 1;
        }
        sorted.iter().try_for_each(|stmt| write!(f, "{}", StmtFormatter{depth:0, max_line_count, stmt}))
    }
}

impl std::fmt::Debug for Domain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Domain({}):\n", self.filepath)?;
    
        let mut sorted = self.tasks.values().collect::<Vec<&Stmt>>();
        let mut largest_line = 0;
        sorted.sort_by(|a, b| {
            let a_line = a.line_no();
            let b_line = b.line_no();
            let r = a_line.cmp(&b_line); 
            largest_line = match r {
                std::cmp::Ordering::Less => b_line,
                std::cmp::Ordering::Equal |
                std::cmp::Ordering::Greater => a_line,
            };
            r
        });
        let mut max_line_count = 0;
        while largest_line > 0 {
            largest_line /= 10;
            max_line_count += 1;
        }
        sorted.iter().try_for_each(|stmt| write!(f, "{:?}", StmtFormatter{depth:0, max_line_count, stmt}))
    }
}

impl Domain {
    fn create_links(tasks:&HashMap<Rc<String>, Stmt>) -> HashMap<Rc<String>, Vec<Rc<String>>> {
        let mut result = HashMap::new();
        for (task_name, task) in tasks.iter() {
            let mut neighbors:Vec<Rc<String>> = Vec::new();
            for (other_name, other) in tasks.iter() {
                if task_name != other_name {
                    let affects = other.affects();
                    task.for_each_method(&mut |method| if affects.intersection(&method.depends()).count() > 0 {
                        println!("{} enables {}.{}", other_name, task_name, method.name().unwrap());
                        neighbors.push(method.name()?);
                        Ok(())
                    } else { Ok(())});
                    // if task effects other's preconditions
                    if affects.intersection(&task.depends()).count() > 0 {
                        neighbors.push(other_name.clone());
                        println!("{} enables {}", other_name, task_name);
                    }
                }
            }
            result.insert(task_name.clone(), neighbors);
            task.for_each_method(&mut |method| {
                let depends = method.depends();
                let mut neighbors:Vec<Rc<String>> = Vec::new();
                for (other_name, other) in tasks.iter() {
                    if other.affects().intersection(&depends).count() > 0 {
                        neighbors.push(other_name.clone())
                    }
                }
                result.insert(method.name().unwrap(), neighbors);
                Ok(())
            });
        }
        result
    }

    pub fn print_parse_error(&self, error: &parser::Error) {
        Parser::print_parse_errors(error, &self.content, &self.filepath);
    }

    pub fn from_file(filepath:&str) -> Result<Domain, Error> {
        let content = fs::read_to_string(filepath)?;
        let ast = Parser::parse(content.as_str())?;
        // errors.iter().for_each(|e| Parser::print_parse_errors(e, content.as_str(), filepath));
        let mut tasks = HashMap::new();
        let mut main = None;
        for stmt in ast {
            let name = stmt.name()?;
            if name.deref().eq("Main") {
                main = Some(name.clone());
            }
      
            // stmt.for_each_method(&mut |method| {tasks.insert(method.name().unwrap().clone(), method.clone());});
            tasks.insert(name, stmt);
            
        }
    
        let neighbors = Domain::create_links(&tasks);
        let domain = Domain{tasks, main:main.unwrap(), neighbors, content, filepath:String::from(filepath)};
        Ok(domain)
    }
}