use std::{fs, fmt::Debug,  collections::HashMap, rc::Rc, ops::Deref};
use super::parser::{Parser, Stmt, ParserError, StmtFormatter};

#[derive(Debug)]

pub struct Domain {
    pub tasks: HashMap<Rc<String>, Stmt>,
    pub neighbors: HashMap<Rc<String>, Vec<Rc<String>>>,
    pub main: Rc<String>,
    pub filepath: String,
    pub content: String,
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
                        neighbors.push(method.name().unwrap())
                    });
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
            });
        }
        result
    }

    pub fn print_parse_error(&self, error: &ParserError) {
        Parser::print_parse_errors(error, &self.content, &self.filepath);
    }

    pub fn from_file(filepath:&str) -> Result<Domain, ParserError> {
        let content = fs::read_to_string(filepath).expect("File error:");
        let (ast, errors) = Parser::parse(content.as_str());
        errors.iter().for_each(|e| Parser::print_parse_errors(e, content.as_str(), filepath));
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