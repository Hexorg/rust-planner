use std::collections::HashSet;
use std::mem;
use std::{fs, fmt::Debug,  collections::HashMap};

use super::optimization::{self, Inertia};
use super::parser::expression::ExpressionVisitor;
use super::parser::statement::StatementVisitor;
use super::parser::{Parser, statement::Stmt, tokens::{self, Token, TokenData}, expression::Expr};
use super::parser;


pub enum NeighborDetectionAlgorithm {
    Inertia,
    VariableSetIntersection,
    FullyLinked,
}

pub enum HeuristicAlgorithm {
    ManhattanDistance,
    None
}

pub struct DomainConfig {
    pub neighbor_detection:NeighborDetectionAlgorithm,
    pub heuristic_algorithm: HeuristicAlgorithm,
}

impl DomainConfig {
    pub fn new() -> Self {
        Self{neighbor_detection:NeighborDetectionAlgorithm::FullyLinked, heuristic_algorithm:HeuristicAlgorithm::None}
    }
    pub fn optimize_max(&mut self) {
        self.neighbor_detection = NeighborDetectionAlgorithm::Inertia;
        self.heuristic_algorithm = HeuristicAlgorithm::ManhattanDistance;
    }


}


#[derive(Debug)]
pub struct PrimitiveTask {
    pub preconditions: Vec<Operation>,
    pub cost: i32,
    pub body: Vec<Operation>,
    pub effects: Vec<Operation>,
    wants: HashMap<usize, Inertia>,
    provides: HashMap<usize, Inertia>,
}
#[derive(Debug)]
pub struct ComplexTask {
    pub preconditions: Vec<Operation>,
    pub cost: i32,
    pub body: Vec<PrimitiveTask>,
    pub effects: Vec<Operation>,
    wants: HashMap<usize, Inertia>,
    provides: HashMap<usize, Inertia>,
}

#[derive(Debug)]
pub enum Task {
    Complex(ComplexTask),
    Primitive(PrimitiveTask),
}

impl Task {
    pub fn get_state_effects(&self) -> HashSet<usize> {
        let mut result = HashSet::new();
        self.effects().iter().for_each(|e| if let Operation::WriteState(idx) = e { result.insert(*idx); });
        result
    }

    pub fn get_state_depends(&self) -> HashSet<usize> {
        let mut result = HashSet::new();
        self.preconditions().iter().for_each(|p| if let Operation::ReadState(idx) = p { result.insert(*idx); });
        result
    }

    pub fn get_wants(&self) -> &HashMap<usize, Inertia> {
        match self {
            Self::Complex(ComplexTask{wants,..}) | 
            Self::Primitive(PrimitiveTask{wants,..}) => wants
        }
    }

    pub fn get_provides(&self) -> &HashMap<usize, Inertia> {
        match self {
            Self::Complex(ComplexTask{provides,..}) | 
            Self::Primitive(PrimitiveTask{provides,..}) => provides
        }
    }

    pub fn preconditions(&self) -> &Vec<Operation> {
        match self {
            Self::Complex(ComplexTask {preconditions,..}) |
            Self::Primitive(PrimitiveTask {preconditions,..}) => preconditions
        }
    }

    pub fn effects(&self) -> &Vec<Operation> {
        match self {
            Self::Complex(ComplexTask {effects,..}) |
            Self::Primitive(PrimitiveTask {effects,..}) => effects
        }
    }
}




/// Structure that holds parsed out AST as well as optimization data
pub struct Domain {
    pub filepath: String,
    pub tasks: Vec<Task>,
    // pub variable_ids: HashMap<String, usize>,
    pub neighbors: HashMap<usize, Vec<usize>>,
    pub config: DomainConfig,
    main_id: Option<usize>,
    compiler: ExpressionCompiler,
    pass_count: usize,
    methods: Vec<PrimitiveTask>,    
}

#[derive(Debug)]
pub enum Error {
    IO(String, std::io::Error),
    Parser(String, Vec<parser::Error>),
    Include(String, Box<Error>),
    Domain(String, String),
}


impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IO(s, e) => write!(f, "{}: {}", s, e),
            Self::Domain(s, e) => write!(f, "{}: {}", s, e),
            Self::Include(s, e) => write!(f, "In file included from {}:\n{}", s, e),
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

impl Error {
    fn set_path(&mut self, filename:&str) {
        match self {
            Self::IO(ref mut f, _) |
            Self::Domain(ref mut f, _) |
            Self::Include(ref mut f, _) |
            Self::Parser(ref mut f, _) => f.extend(filename.chars())
        }
    }

    fn has_path(&self) -> bool {
        match self {
            Self::IO(p, _) | 
            Self::Domain(p, _) |
            Self::Include(p, _) |
            Self::Parser(p, _) => p.len() > 0
        }
    }
}


impl std::error::Error for Error { }

impl From<std::io::Error> for Error {
    fn from(arg: std::io::Error) -> Self {
        Self::IO(String::new(), arg)
    }
}

impl From<parser::Error> for Error {
    fn from(arg: parser::Error) -> Self {
        Self::Parser(String::new(), vec![arg])
    }
}

impl From<Vec<parser::Error>> for Error {
    fn from(arg: Vec<parser::Error>) -> Self {
        Self::Parser(String::new(), arg)
    }
}

impl std::fmt::Debug for Domain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Domain({}):", self.filepath)?;
        writeln!(f, "State mapping: {:?}", self.compiler.state_mapping)?;
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






impl StatementVisitor<(), Error> for Domain {
    fn visit_task_declaration(&mut self, name:&[Token]) -> Result<(), Error> {
        println!("Declaring task: {}", name.iter().map(|t| t.unwrap_identifier()).fold(String::new(), |acc,item| acc + item + "."));
        Ok(())
    }
    fn visit_task(&mut self, name:&[Token], preconditions:Option<&Expr>, cost:Option<&Expr>, binding:Option<(&str, &str)>, body:&Stmt, effects:Option<&Stmt>, planning:Option<&Stmt>) -> Result<(), Error> {
        if self.pass_count == 0 { // on the first pass we figure out task name mapping only, so that we can differentiate between operators and tasks
            let task_id = self.compiler.task_mapping.len();
            self.compiler.task_mapping.insert(name.to_owned(), task_id).and_then(|_| Some(Err(token.to_err("Duplicate task name.")))).unwrap_or(Ok(()))?;
            if name == "Main" {
                self.main_id = Some(task_id);
            }
            Ok(())
        } else {
            // println!("Building task {}", name);
            if let Some(Binding{class_type, variable_name}) = binding {
                self.compiler.binding = Some(Binding{class_type:class_type.clone(), variable_name:variable_name.clone()});
                let count = if let Some(v) = self.compiler.type_map.get(class_type) { v.len() } else {
                    return Err(token.to_err("Undefined binding class in this task.").into());
                };
                for n in 0..count {
                    self.compiler.substitution_id = n;
                    self.build_task(preconditions, cost, body, effects)?;
                }
                Ok(())
            } else {
                self.compiler.binding = None;
                self.build_task(preconditions, cost, body, effects)
            }   
        }
    }

    fn visit_block(&mut self, block:&[Stmt]) -> Result<(), Error> {
        block.iter().try_for_each(|stmt| stmt.accept(self))
    }

    fn visit_expression(&mut self, expr:&Expr) -> Result<(), Error> {
        expr.accept(&mut self.compiler)
    }

    fn visit_type(&mut self, token:&Token, body:&Stmt) -> Result<(), Error> {
        if self.pass_count == 0 {
            self.compiler.is_class_definition = Some(String::from(token.unwrap_identifier()));
            body.accept(self)?;
            self.compiler.is_class_definition = None;
        }
        Ok(())
    }

    fn visit_include(&mut self, token:&Token) -> Result<(), Error> {
        let filepath = token.unwrap_literal().unwrap_str();
        if self.pass_count == 0 {
            match self.compile(filepath, true) {
                Ok(_) => Ok(()),
                Err(mut e) => {if !e.has_path() { e.set_path(filepath); } Err(Error::Include(filepath.to_owned(), Box::new(e)))} 
            }    
        } else {
            Ok(())
        }
    }
}



impl Domain {
    fn build_task(&mut self, preconditions:Option<&Expr>, cost:Option<i32>, body:&Stmt, effects:Option<&Stmt>) -> Result<(), Error> {
        preconditions.and_then(|expr| Some(expr.accept(&mut self.compiler))).unwrap_or_else(|| Ok(self.compiler.bytecode.push(Operation::Push(OperandType::B(true)))))?;
        let preconditions = mem::take(&mut self.compiler.bytecode);
        let wants = optimization::build_wants(&preconditions)?;
        effects.and_then(|stmt| Some(stmt.accept(self))).unwrap_or(Ok(()))?;
        let effects = mem::take(&mut self.compiler.bytecode);
        let provides = optimization::build_provides(&effects, &wants)?;
        self.compiler.is_body = true;
        body.accept(self)?;
        self.compiler.is_body = false;
        if self.methods.len() > 0 {
            if self.compiler.bytecode.len() > 0 {
                return Err(body.to_err("Can not use methods and operators at the same time in this task.").into())
            }
            let body = mem::take(&mut self.methods);
            self.tasks.push(Task::Complex(ComplexTask{preconditions, cost:cost.unwrap_or(0), body, effects, wants, provides}))
        } else {
            let body = mem::take(&mut self.compiler.bytecode);
            self.tasks.push(Task::Primitive(PrimitiveTask{preconditions, cost:cost.unwrap_or(0), body, effects, wants, provides}))
        }
        Ok(())
    }
    fn build_neightbor_map_fully_linked(&mut self) {
        for i in 0..self.tasks.len() {
            self.neighbors.insert(i, (0..self.tasks.len()).collect());
        }
    }

    /// Figure out which tasks can follow what, by checking which tasks effect variables
    /// that exist in other tasks' preconditions
    fn build_neighbor_map_based_on_variable_intersection(&mut self) {
        for (i, x) in self.tasks.iter().enumerate() {
            let effects = x.get_state_effects();
            let mut to_vec = Vec::new();
            for (iy, y) in self.tasks.iter().enumerate() {
                if iy != i {
                    if effects.intersection(&y.get_state_depends()).count() > 0 {
                        to_vec.push(iy);
                    }
                }
            }
            self.neighbors.insert(i, to_vec);
        }
    }

    /// Figure out which tasks can follow what, by checking which tasks effects provide wants
    /// that other tasks' preconditions want.
    fn build_neighbor_map_based_on_inertia(&mut self) {
        for (i, x) in self.tasks.iter().enumerate() {
            let x_provides = x.get_provides();

            let mut to_vec = Vec::new();
            for (iy, y) in self.tasks.iter().enumerate() {
                if iy != i {
                    let y_wants = y.get_wants();
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
            self.neighbors.insert(i, to_vec);
        }
    }

    fn compile(&mut self, filepath:&str, is_include:bool) -> Result<(), Error> {
        let content = fs::read_to_string(filepath)?;
        let current_pass_count = self.pass_count;
        self.pass_count = 0;
        for stmt in Parser::new(content.as_str()) {
            stmt?.accept(self)?
        }
        self.pass_count = 1;
        for stmt in Parser::new(content.as_str()) {
            stmt?.accept(self)?
        }
        self.pass_count = current_pass_count;
        if !is_include && self.main_id.is_none() {
            Err(Error::Domain(filepath.to_owned(), "Main task not found.".to_string()))
        } else {
            Ok(())
        }
    }

    pub fn get_main_id(&self) -> usize {
        // check in compile ensures this never fails.
        self.main_id.unwrap()
    }

    pub fn get_state_mapping(&self) -> &HashMap<String, usize> {
        &self.compiler.state_mapping
    }

    pub fn get_operator_mapping(&self) -> Vec<&String> {
        let mut container: Vec<_> = self.compiler.operator_mapping.keys().collect();
        container.sort_by_key(|a| self.compiler.operator_mapping.get(*a).unwrap());
        container
    }

    pub fn get_blackboard_mapping(&self) -> Vec<&String> {
        let mut container: Vec<_> = self.compiler.blackboard_mapping.keys().collect();
        container.sort_by_key(|a| self.compiler.blackboard_mapping.get(*a).unwrap());
        container
    }
       
    pub fn from_file(filepath:&str, type_counts:HashMap<&str, Vec<&str>>) -> Result<Domain, Error> {   
        let mut type_map = HashMap::<String, Vec<String>>::new();
        type_map.extend(type_counts.iter().map(|(k, v)| (String::from(*k), v.iter().map(|s| String::from(*s)).collect())));

        let mut compiler = ExpressionCompiler::new();
        compiler.type_map = type_map;
        let mut domain = Domain{
            filepath:String::from(filepath), 
            tasks:Vec::new(), 
            compiler, 
            config: DomainConfig::new(),
            methods:Vec::new(), 
            neighbors:HashMap::new(), 
            pass_count:0, 
            main_id:None,
        };


        match domain.compile(filepath, false) {
            Ok(_) => {
                domain.config.neighbor_detection = NeighborDetectionAlgorithm::VariableSetIntersection;
                domain.config.heuristic_algorithm = HeuristicAlgorithm::ManhattanDistance;
                domain.config.optimize_max();
                match domain.config.neighbor_detection {
                    NeighborDetectionAlgorithm::Inertia => domain.build_neighbor_map_based_on_inertia(),
                    NeighborDetectionAlgorithm::VariableSetIntersection => domain.build_neighbor_map_based_on_variable_intersection(),
                    NeighborDetectionAlgorithm::FullyLinked => domain.build_neightbor_map_fully_linked()
                }
                Ok(domain)
            },
            Err(mut e) => {if !e.has_path() { e.set_path(filepath);} Err(e)} 
        }
    }
}