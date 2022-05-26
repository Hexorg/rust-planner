use std::{fs, fmt::Debug,  collections::HashMap, rc::Rc};
use super::parser::{Parser, Stmt, ParserError, StmtFormatter};

#[derive(Debug)]

pub struct Domain {
    pub tasks: HashMap<Rc<String>, Stmt>,
    pub neighbors: HashMap<Rc<String>, Vec<Rc<String>>>,
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
                    task.for_each_method(|method| if affects.intersection(&method.depends()).count() > 0 {
                        neighbors.push(method.name().unwrap())
                    });
                    // if task effects other's preconditions
                    if affects.intersection(&other.depends()).count() > 0 {
                        neighbors.push(other.name().unwrap());
                        // task.neighbors.push(other_name.clone());
                        println!("{} enables {}", task_name, other_name);
                    }
                }
            }
            result.insert(task_name.clone(), neighbors);
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
        for stmt in ast {
            tasks.insert(stmt.name()?, stmt);
        }
        let neighbors = Domain::create_links(&tasks);
        let domain = Domain{tasks, neighbors, content, filepath:String::from(filepath)};
        Ok(domain)
    }
}