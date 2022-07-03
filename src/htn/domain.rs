use std::hash::Hash;
use std::{fs, fmt::Debug,  collections::HashMap};

// use super::optimization::{self, Inertia};
use super::compiler::{Task, self};
use super::parser::Parser;
use super::parser;


/// Structure that holds parsed out AST as well as optimization data
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

#[derive(Debug)]
pub enum Error {
    Basic(String,String),
    Parser(String, Vec<parser::Error>),
    FromFile(String, Box<Error>),
}


impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Basic(s, e) => write!(f, "{}: {}", s, e),
            Self::FromFile(s, e) => write!(f, "In file included from {}:\n{}", s, e),
            Self::Parser(filepath, es) => {
                let htn_source = fs::read_to_string(filepath).unwrap();
                let mut lines = htn_source.lines();
                let mut last_error_line = 0;
                for e in es {
                    if let Some(eline) = lines.nth(e.line - last_error_line-1) {
                        let line_number_string = format!("{}", e.line);
                        writeln!(f, "{}:{} Error:", filepath, e.line)?; 
                        writeln!(f, "\t{}: {}", line_number_string, eline)?;
                        last_error_line = e.line;
                        let debug_str_col_pos = line_number_string.len() + 1 + e.col;
                        writeln!(f, "\t{:->width$} {}\n",'^', e.message, width=debug_str_col_pos)?; 
                    }
                }
                Ok(())
            }
        }
    }
}

// impl Error {
//     fn set_path(&mut self, filename:&str) {
//         match self {
//             Self::IO(ref mut f, _) |
//             Self::Domain(ref mut f, _) |
//             Self::Include(ref mut f, _) |
//             Self::Parser(ref mut f, _) => f.extend(filename.chars())
//         }
//     }

//     fn has_path(&self) -> bool {
//         match self {
//             Self::IO(p, _) | 
//             Self::Domain(p, _) |
//             Self::Include(p, _) |
//             Self::Parser(p, _) => p.len() > 0
//         }
//     }
// }


impl std::error::Error for Error { }

// impl From<std::io::Error> for Error {
//     fn from(arg: std::io::Error) -> Self {
//         Self::IO(String::new(), arg)
//     }
// }

// impl From<parser::Error> for Error {
//     fn from(arg: parser::Error) -> Self {
//         Self::Parser(String::new(), vec![arg])
//     }
// }

// impl From<Vec<parser::Error>> for Error {
//     fn from(arg: Vec<parser::Error>) -> Self {
//         Self::Parser(String::new(), arg)
//     }
// }

impl std::fmt::Debug for Domain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Domain({}):", self.filepath)?;
        writeln!(f, "State mapping: {:?}", self.state_mapping)?;
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
    // fn build_neightbor_map_fully_linked(&mut self) {
    //     for i in 0..self.tasks.len() {
    //         self.neighbors.insert(i, (0..self.tasks.len()).collect());
    //     }
    // }

    // /// Figure out which tasks can follow what, by checking which tasks effect variables
    // /// that exist in other tasks' preconditions
    // fn build_neighbor_map_based_on_variable_intersection(&mut self) {
    //     for (i, x) in self.tasks.iter().enumerate() {
    //         let effects = x.get_state_effects();
    //         let mut to_vec = Vec::new();
    //         for (iy, y) in self.tasks.iter().enumerate() {
    //             if iy != i {
    //                 if effects.intersection(&y.get_state_depends()).count() > 0 {
    //                     to_vec.push(iy);
    //                 }
    //             }
    //         }
    //         self.neighbors.insert(i, to_vec);
    //     }
    // }

    /// Figure out which tasks can follow what, by checking which tasks effects provide wants
    /// that other tasks' preconditions want.
    fn build_neighbor_map_based_on_inertia(tasks: &Vec<Task>) -> HashMap<usize, Vec<usize>> {
        let mut result = HashMap::new();
        for (i, x) in tasks.iter().enumerate() {
            let x_provides = x.provides();

            let mut to_vec = Vec::new();
            for (iy, y) in tasks.iter().enumerate() {
                if iy != i {
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
        result
    }

    pub fn main(&self) -> &Task {
        &self.tasks[self.main_id]
    }

       
    pub fn from_file(filepath:&str, type_counts:HashMap<&str, Vec<&str>>) -> Result<Domain, Error> {   
        // let mut type_mapping = HashMap::<String, Vec<String>>::new();
        // type_mapping.extend(type_counts.iter().map(|(k, v)| (String::from(*k), v.iter().map(|s| String::from(*s)).collect())));
        let mut type_mapping = HashMap::<&str, Vec<&str>>::new();
        type_mapping.extend(type_counts);
        let mut state_mapping = HashMap::new();
        let mut blackboard_mapping = HashMap::new();
        let mut task_mapping = HashMap::new();
        let mut operator_mapping = HashMap::new();
        let mut compiler = compiler::domain::DomainCompiler::new(&mut blackboard_mapping, &mut task_mapping, &mut operator_mapping, &mut type_mapping, &mut state_mapping);
        match fs::read_to_string(filepath) {
            Ok(code) => {
                let mut errors = Vec::<parser::Error>::new();
                for result in Parser::new(code.as_str()) {
                    match result {
                        Ok(stmt) => match stmt.accept(&mut compiler) {
                            Ok(()) => (),
                            Err(e) => errors.push(e),
                        },
                        Err(e) => errors.push(e),
                    }
                }
                if errors.len() > 0 {
                    Err(Error::Parser(filepath.to_owned(), errors))
                } else {
                    let tasks = compiler.finish();
                    if task_mapping.contains_key("Main") {
                        let main_id = task_mapping["Main"];
                        let neighbors = Domain::build_neighbor_map_based_on_inertia(&tasks);
                        Ok(Domain{filepath:filepath.to_owned(), tasks, main_id, state_mapping, blackboard_mapping, task_mapping, operator_mapping, neighbors})
                    } else {
                        Err(Error::Basic(filepath.to_owned(), "Main task not declared.".to_owned()))
                    }
                    
                }
            },
            Err(e) => Err(Error::Basic(filepath.to_owned(), e.to_string())),
        }
    }
}