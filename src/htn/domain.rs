use std::collections::HashMap;

// use super::optimization::{self, Inertia};
use super::compiler::{Task, self, TaskBody};
use super::parser::statement::StatementVisitor;
use super::parser::tokens::{Token, TokenData, Literal};

pub use compiler::Error;

/// Structure that holds compiled Tasks as well as optimization data
pub struct Domain {
    pub filepath: String,
    tasks: Vec<Task>,
    neighbors: HashMap<usize, Vec<usize>>,
    main_id: usize,   
    state_mapping: HashMap<String, usize>,
    blackboard_mapping: HashMap<String, usize>,
    operator_mapping: HashMap<String, usize>,
    task_mapping: HashMap<String, usize>,
}

impl std::fmt::Debug for Domain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Domain({}):", self.filepath)?;
        writeln!(f, "State mapping: {:?}", self.state_mapping)?;
        writeln!(f, "Task mapping: {:?}", self.task_mapping)?;
        let total_links = self.neighbors.values().fold(0, |acc, item| acc + item.len());
        writeln!(f, "Total links: {}", total_links)?;
        writeln!(f, "Task neighbors: {:?}", self.neighbors)?;
        writeln!(f, "Tasks:")?;
        for (id, task) in self.tasks.iter().enumerate() {
            writeln!(f, "{}: {:?}", id, task)?;
        }
        Ok(())
    }
}


impl Domain {
    /// Figure out which tasks can follow what, by checking which tasks effects provide wants
    /// that other tasks' preconditions want.
    fn build_neighbor_map_based_on_inertia(tasks: &Vec<Task>) -> HashMap<usize, Vec<usize>> {
        let mut result = HashMap::new();
        for (i, x) in tasks.iter().enumerate() {
            if !x.is_method {
                let x_provides = x.provides();

                let mut to_vec = Vec::new();
                for (iy, y) in tasks.iter().enumerate() {
                    if iy != i && !y.is_method {
                        let y_wants = y.wants();
                        let mut should_add = true;
                        for (xid, provides) in x_provides {
                            if y_wants.contains_key(xid) {
                                if y_wants.get(xid).unwrap().satisfies(provides) {
                                    should_add = true
                                } else {
                                    should_add = false;
                                    break
                                }
                            }
                        }
                        if should_add {
                            to_vec.push(iy);
                        }
                    }
                }
                result.insert(i, to_vec);
            }
        }
        result
    }

    pub fn main(&self) -> &Task {
        &self.tasks[self.main_id]
    }

    pub fn main_id(&self) -> usize {
        self.main_id
    }

    pub fn tasks(&self) -> &[Task] {
        &self.tasks
    }

    pub fn state_mapping(&self) -> &HashMap<String, usize> {
        &self.state_mapping
    }

    pub fn task_mapping(&self) -> &HashMap<String, usize> {
        &self.task_mapping
    }

    pub fn operator_mapping(&self) -> &HashMap<String, usize> {
        &self.operator_mapping
    }

    pub fn operator_vec(&self) -> Vec<String> {
        let mut operator_vec: Vec<String> = self.operator_mapping.iter().map(|(k,_v)| k.clone()).collect();
        operator_vec.sort_by(|l,r| self.operator_mapping[l].cmp(&self.operator_mapping[r]));
        operator_vec
    }

    pub fn blackboard_mapping(&self) -> &HashMap<String, usize> {
        &self.blackboard_mapping
    }

    pub fn blackboard_vec(&self) -> Vec<String> {
        let mut blackboard_vec: Vec<String> = self.blackboard_mapping.iter().map(|(k,_v)| k.clone()).collect();
        blackboard_vec.sort_by(|l,r| self.blackboard_mapping[l].cmp(&self.blackboard_mapping[r]));
        blackboard_vec
    }

    pub fn neighbors(&self) -> &HashMap<usize, Vec<usize>> {
        &self.neighbors
    }

       
    pub fn from_file(filepath:&str, type_counts:HashMap<&str, Vec<&str>>) -> Result<Domain, Error> {   
        // let mut type_mapping = HashMap::<String, Vec<String>>::new();
        // type_mapping.extend(type_counts.iter().map(|(k, v)| (String::from(*k), v.iter().map(|s| String::from(*s)).collect())));
        let mut type_mapping = HashMap::<String, Vec<String>>::new();
        type_mapping.extend(type_counts.iter().map(|(k,v)| (String::from(*k), v.iter().map(|s| String::from(*s)).collect())));
        let mut state_mapping = HashMap::new();
        let mut blackboard_mapping = HashMap::new();
        let mut task_mapping = HashMap::new();
        let mut operator_mapping = HashMap::new();
        let mut compiler = compiler::domain::DomainCompiler::new(&mut blackboard_mapping, &mut task_mapping, &mut operator_mapping, &mut type_mapping, &mut state_mapping);
        compiler.visit_include(&Token{line:0, col:0, len:0, t:TokenData::Literal(Literal::S(filepath))})?;
        let tasks = compiler.finish();
        if task_mapping.contains_key("Main") {
            let main_id = task_mapping["Main"];
            let neighbors = Domain::build_neighbor_map_based_on_inertia(&tasks);
            Ok(Domain{filepath:filepath.to_owned(), tasks, main_id, state_mapping, blackboard_mapping, task_mapping, operator_mapping, neighbors})
        } else {
            Err(Error::Basic(filepath.to_owned(), "Main task not declared.".to_owned()))
        }
    }
}