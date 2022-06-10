use std::collections::HashSet;
use std::{fs, fmt::Debug,  collections::HashMap, rc::Rc, ops::Deref};
use super::parser::{Parser, Stmt, StmtFormatter};
use super::parser;

/// Structure that holds parsed out AST as well as optimization data
pub struct Domain {
    pub filepath: String,
    pub tasks: Vec<Stmt>,
    pub variable_ids: HashMap<String, usize>,
    neighbors: HashMap<String, Vec<String>>,
    task_ids: HashMap<String, usize>,
    main_id: usize,
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
    fn get_neighbors(tasks:&Vec<Stmt>) -> Result<HashMap<String, Vec<String>>, Error> {
        let mut result = HashMap::new();
        for (i, x) in tasks.iter().enumerate() {
            let writes = if let Some(e) = x.effects()? {
                e.expressions()?.fold(HashSet::new(), |mut acc,item| {if let Some(w) = item.get_writes() { acc.insert(w); } acc})
            } else {
                HashSet::new()
            };
            let mut to_vec = Vec::new();
            for (iy, y) in tasks.iter().enumerate() {
                if iy != i {
                    if let Some(p) = y.preconditions()? {
                        if writes.intersection(&p.get_reads()).count() > 0 {
                            to_vec.push(String::from(y.name()?));
                            
                        }
                    }
                }
            }
            result.insert(String::from(x.name()?), to_vec);
        }
        Ok(result)
    }

    fn variable_strings_to_ids(tasks:&mut Vec<Stmt>) -> Result<HashMap<String, usize>, Error> {
        let mut state_variables = HashMap::new();
        // First build a map of variable names to IDs
        for task in tasks.iter() {
            if let Some(preconditions) = task.preconditions()? {
                for var in preconditions.get_reads() {
                    if !state_variables.contains_key(var) {
                        state_variables.insert(String::from(var), state_variables.len());
                    }
                }
            }
            if task.is_composite()? {
                for method in task.methods()? {
                    if let Some(preconditions) = method.preconditions()? {
                        for var in preconditions.get_reads() {
                            if !state_variables.contains_key(var) {
                                state_variables.insert(String::from(var), state_variables.len());
                            }
                        }
                    }
                }
            }
        }

        // Set var IDs 
        for task in tasks.iter_mut() {
            if let Some(preconditions) = task.preconditions_mut()? {
                preconditions.set_var_ids(&state_variables);
            }
            if let Some(cost) = task.cost_mut()? {
                cost.set_var_ids(&state_variables)
            }
            if task.is_composite()? {
                for method in task.methods_mut()? {
                    if let Stmt::Method{preconditions:Some(preconditions),..} = method {
                        preconditions.set_var_ids(&state_variables);
                    }
                }
            }
        }

        // Check if we have unsed effects
        for task in tasks.iter_mut() {
            if let Some(effects) = task.effects_mut()? {
                for op in effects.expressions()? {
                    if let Some(effect) = op.get_writes() {
                        if !state_variables.contains_key(effect) {
                            return Err(op.to_err(String::from("Effect assigned but never used")).into())
                        }
                    }
                }
                for op in effects.expressions_mut()? {
                    if let Stmt::Expression(e) = op {
                        e.set_var_ids(&state_variables);
                    }
                    
                }
            }
        }

        Ok(state_variables)
    }

    pub fn get_task(&self, name:&str) -> Option<&Stmt> {
        if let Some(id) = self.task_ids.get(name) {
            self.tasks.get(*id)
        } else {
            None
        }
        
    }

    pub fn get_main(&self) -> &Stmt {
        self.tasks.get(self.main_id).unwrap()
    } 

    pub fn get_enablers_of(&self, key:&str) -> Vec<&str> {
        if let Some(v) = self.neighbors.get(key) {
            let r:Vec<&str> = v.iter().map(|i| i.as_str()).collect();
            r
        } else {
            self.get_all_task_names().collect()
        }
    }

    pub fn get_all_task_names(&self) -> impl Iterator<Item = &str> {
        self.task_ids.keys().map(|key| key.as_str())
    }

    pub fn from_file(filepath:&str) -> Result<Domain, Error> {
        let content = fs::read_to_string(filepath)?;
        let mut tasks = Parser::parse(content.as_str())?;
        // errors.iter().for_each(|e| Parser::print_parse_errors(e, content.as_str(), filepath));
        let mut task_ids = HashMap::new();
        let mut main_id = None;
        for (idx, stmt) in tasks.iter().enumerate() {
            let name = stmt.name()?;
            if name.eq("Main") {
                main_id = Some(idx);
            }
            task_ids.insert(String::from(name), idx);
            
        }
        let neighbors = Domain::get_neighbors(&tasks)?;
        let variable_ids = Domain::variable_strings_to_ids(&mut tasks)?;
        let domain = Domain{tasks, task_ids, main_id:main_id.expect("Domain must contain 'Main' task"), variable_ids, filepath:String::from(filepath), neighbors};
        Ok(domain)
    }
}