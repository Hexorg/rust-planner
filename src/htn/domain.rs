use std::collections::HashSet;
use std::hash::Hash;
use std::mem;
use std::{fs, fmt::Debug,  collections::HashMap, rc::Rc, ops::Deref};

use super::parser::expression::ExpressionVisitor;
use super::parser::statement::StatementVisitor;
use super::parser::{Parser, statement::{Stmt, Binding}, tokens::{self, Token, TokenData}, expression::Expr};
use super::parser;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum OperandType {
    I(i32),
    F(f32),
    B(bool)
}

impl std::cmp::Eq for OperandType {

}

impl OperandType {
    #[inline]
    pub fn is_true(&self) -> bool {
        match self {
            Self::I(i) => *i == 1,
            Self::B(b) => *b,
            Self::F(_) => false
        }
    }
}

impl Default for OperandType {
    fn default() -> Self {
        Self::I(0)
    }
}

impl std::hash::Hash for OperandType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            OperandType::I(i) => i.hash(state),
            OperandType::F(f) => f.to_bits().hash(state),
            OperandType::B(b) => b.hash(state),
        }
    }
}


#[derive(Clone, Copy, Debug)]
pub enum Operation {
    ReadState(usize),
    WriteState(usize),
    Push(OperandType),
    Equals,
    Greater,
    Smaller,
    GreaterOrEquals,
    SmallerOrEquals,
    Not, 
    And,
    Or,
    Subtract,
    Add,
    Multiply,
    Divide,
    ReadBlackboard(usize),
    WriteBlackboard(usize),
    PlanTask(usize),
    CallOperator(usize, usize), // (operator_id, arity)
}
#[derive(Debug)]
pub struct PrimitiveTask {
    pub preconditions: Vec<Operation>,
    pub cost: i32,
    pub body: Vec<Operation>,
    pub effects: Vec<Operation>
}
#[derive(Debug)]
pub struct ComplexTask {
    pub preconditions: Vec<Operation>,
    pub cost: i32,
    pub body: Vec<PrimitiveTask>,
    pub effects: Vec<Operation>,
    task_ids: HashMap<String, usize>,
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


struct ExpressionCompiler {
    pub bytecode:Vec<Operation>,
    pub substitution: Option<String>,
    pub substitution_count: usize,
    pub is_body: bool,
    pub state_mapping:HashMap<String, usize>,
    pub blackboard_mapping:HashMap<String, usize>,
    pub operator_mapping:HashMap<String, usize>,
    task_mapping: HashMap<String, usize>,
}

/// Structure that holds parsed out AST as well as optimization data
pub struct Domain {
    pub filepath: String,
    pub tasks: Vec<Task>,
    // pub variable_ids: HashMap<String, usize>,
    pub neighbors: HashMap<usize, Vec<usize>>,
    main_id: Option<usize>,
    compiler: ExpressionCompiler,
    pass_count: usize,
    type_counts:HashMap<String, usize>,
    methods: Vec<PrimitiveTask>,
    method_ids: HashMap<String, usize>,
    
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
        writeln!(f, "Task neighbors: {:?}", self.neighbors)?;
        writeln!(f, "Tasks:")?;
        for (id, task) in self.tasks.iter().enumerate() {
            writeln!(f, "{}: {:?}", id, task)?;
        }
        Ok(())
    }
}



impl ExpressionCompiler {
    pub fn new() -> Self {
        Self { bytecode: Vec::new(), state_mapping: HashMap::new(), blackboard_mapping: HashMap::new(), is_body:false, operator_mapping: HashMap::new(), task_mapping:HashMap::new(), substitution:None, substitution_count:0}
    }
    #[inline]
    fn get_varpath_state_idx(&mut self, var_path:&[Token]) -> usize {
        let mut iter = var_path.iter();

        let first = iter.by_ref().take(1).map(|t| {
                if let Token{t:TokenData::Label(s),..} = t {
                    if let Some(sub) = &self.substitution {
                        if s == sub {
                            format!("{}{}", sub, self.substitution_count)
                        } else {
                            s.clone()
                        }
                    } else {
                        s.clone()
                    }
                } else {
                    "Default".to_owned()
                }
            }).fold(String::new(), |acc, item| acc + item.as_str());

        let vname = iter.map(|t| if let TokenData::Label(s) = &t.t { s.as_str() } else { "" }).fold(first, |acc,item| acc + "." + item);
        if self.state_mapping.contains_key(&vname) {
            self.state_mapping[&vname]
        } else {
            let s = self.state_mapping.len();
            self.state_mapping.insert(vname, s);
            s
        }
    }
    #[inline]
    fn get_varpath_blackboard_idx(&mut self, var_path:&[Token]) -> usize {
        let vname = var_path.iter().map(|t| if let TokenData::Label(s) = &t.t { s.as_str() } else { "" }).fold(String::new(), |acc,item| acc + "." + item);
        if self.blackboard_mapping.contains_key(&vname) {
            self.blackboard_mapping[&vname]
        } else {
            let s = self.blackboard_mapping.len();
            self.blackboard_mapping.insert(vname, s);
            s
        }
    }
}

impl ExpressionVisitor<(), Error> for ExpressionCompiler {
    fn visit_binary_expr(&mut self, token: &Token, left: &Expr, right: &Expr) -> Result<(), Error> {
        left.accept(self)?;
        right.accept(self)?;
        use TokenData::*;
        match token.t {
            NotEquals => {self.bytecode.push(Operation::Not); Ok(self.bytecode.push(Operation::Equals))},
            EqualsEquals => Ok(self.bytecode.push(Operation::Equals)),
            Smaller => Ok(self.bytecode.push(Operation::Smaller)),
            SmallerOrEquals => Ok(self.bytecode.push(Operation::SmallerOrEquals)),
            Greater => Ok(self.bytecode.push(Operation::Greater)),
            GreaterOrEquals => Ok(self.bytecode.push(Operation::GreaterOrEquals)),
            And => Ok(self.bytecode.push(Operation::And)),
            Or => Ok(self.bytecode.push(Operation::Or)),
            Minus => Ok(self.bytecode.push(Operation::Subtract)),
            Plus => Ok(self.bytecode.push(Operation::Add)),
            Slash => Ok(self.bytecode.push(Operation::Multiply)),
            Star => Ok(self.bytecode.push(Operation::Divide)),
            _ => Err(token.to_err("Unsupported binary expression token.").into())
        }
    }

    fn visit_grouping_expr(&mut self, _: &Token, group: &Expr) -> Result<(), Error> {
        group.accept(self)
    }

    fn visit_literal_expr(&mut self, token: &Token, data:&tokens::Literal) -> Result<(), Error> {
        match data {
            tokens::Literal::I(i) => Ok(self.bytecode.push(Operation::Push(OperandType::I(*i)))),
            tokens::Literal::F(f) => Ok(self.bytecode.push(Operation::Push(OperandType::F(*f)))),
            tokens::Literal::B(b) => Ok(self.bytecode.push(Operation::Push(OperandType::B(*b)))),
            tokens::Literal::S(_) => Err(token.to_err("Unexpected string literal.").into()),
        }
    }

    fn visit_variable_expr(&mut self, var_path:&[Token]) -> Result<(), Error> {
        if self.is_body {
            let idx = self.get_varpath_blackboard_idx(var_path);
            Ok(self.bytecode.push(Operation::ReadBlackboard(idx)))
        } else {
            let idx = self.get_varpath_state_idx(var_path);
            Ok(self.bytecode.push(Operation::ReadState(idx)))
        }
    }

    fn visit_unary_expr(&mut self, token: &Token, right: &Expr) -> Result<(), Error> {
        match token.t {
            TokenData::Not => {right.accept(self)?; Ok(self.bytecode.push(Operation::Not))}
            _ => Err(token.to_err("Unsupported unary operation.").into())
        }
    }

    fn visit_assignment_expr(&mut self, var_path:&[Token], left:&Expr) -> Result<(), Error> {
        left.accept(self)?;
        if self.is_body {
            let idx = self.get_varpath_blackboard_idx(var_path);
            Ok(self.bytecode.push(Operation::WriteBlackboard(idx)))
        } else {
            let idx = self.get_varpath_state_idx(var_path);
            Ok(self.bytecode.push(Operation::WriteState(idx)))
        }
    }

    fn visit_call_expr(&mut self, _token: &Token, name:&str, args:&[Expr]) -> Result<(), Error> {
        args.iter().try_for_each(|arg| arg.accept(self))?;
        if self.task_mapping.contains_key(name) {
            Ok(self.bytecode.push(Operation::PlanTask(self.task_mapping[name])))
        } else {
            if self.operator_mapping.contains_key(name) {
                Ok(self.bytecode.push(Operation::CallOperator(self.operator_mapping[name], args.len())))
            } else {
                let idx = self.operator_mapping.len();
                self.operator_mapping.insert(name.to_owned(), idx);
                Ok(self.bytecode.push(Operation::CallOperator(idx, args.len())))
            }
        }
    }

    fn visit_nop_expr(&mut self, _: &Token) -> Result<(), Error> {
        Ok(())
    }
}



impl StatementVisitor<(), Error> for Domain {
    fn visit_method(&mut self, token:&Token, name:&str, preconditions:Option<&Expr>, cost:Option<i32>, body:&Stmt, else_cost:Option<i32>, else_body:Option<&Stmt>) -> Result<(), Error> {
        self.method_ids.insert(name.to_owned(), self.method_ids.len()).and_then(|_| Some(Err(token.to_err("Duplicate method name.")))).unwrap_or(Ok(()))?;
        preconditions.and_then(|expr| Some(expr.accept(&mut self.compiler))).unwrap_or_else(|| Ok(self.compiler.bytecode.push(Operation::Push(OperandType::B(true)))))?;
        let preconditions = mem::take(&mut self.compiler.bytecode);
        self.compiler.is_body = true;
        body.accept(self)?;
        self.compiler.is_body = false;
        let body = mem::take(&mut self.compiler.bytecode);
        let effects = Vec::new();
        if let Some(else_body) = else_body {
            let mut else_conditions = preconditions.clone();
            self.methods.push(PrimitiveTask{preconditions, cost:cost.unwrap_or(0), body, effects});
            else_conditions.push(Operation::Not);
            self.method_ids.insert("Dont".to_owned() + name, self.method_ids.len()).and_then(|_| Some(Err(token.to_err("Duplicate generated method name. Else methods are automatically named DontMethod.")))).unwrap_or(Ok(()))?;
            else_body.accept(self)?;
            let body = mem::take(&mut self.compiler.bytecode);
            let effects = Vec::new();
            self.methods.push(PrimitiveTask{preconditions:else_conditions, cost:else_cost.unwrap_or(0), body, effects})
        } else {
            self.methods.push(PrimitiveTask{preconditions, cost:cost.unwrap_or(0), body, effects});
        }
        Ok(())
    }

    fn visit_task(&mut self, token:&Token, name:&str, binding:Option<&Binding>, preconditions:Option<&Expr>, cost:Option<i32>, body:&Stmt, effects:Option<&Stmt>) -> Result<(), Error> {
        if self.pass_count == 0 { // on the first pass we figure out task name mapping only, so that we can differentiate between operators and tasks
            let task_id = self.compiler.task_mapping.len();
            self.compiler.task_mapping.insert(name.to_owned(), task_id).and_then(|_| Some(Err(token.to_err("Duplicate task name.")))).unwrap_or(Ok(()))?;
            if name == "Main" {
                self.main_id = Some(task_id);
            }
            Ok(())
        } else {
            if let Some(Binding{class_type, variable_name}) = binding {
                self.compiler.substitution = Some(variable_name.clone());
                for n in 0..self.type_counts[class_type] {
                    self.compiler.substitution_count = n;
                    self.build_task(preconditions, cost, body, effects)?;
                }
                Ok(())
            } else {
                self.compiler.substitution = None;
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

    fn visit_include(&mut self, token:&Token, filepath:&str) -> Result<(), Error> {
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
        effects.and_then(|stmt| Some(stmt.accept(self))).unwrap_or(Ok(()))?;
        let effects = mem::take(&mut self.compiler.bytecode);
        self.compiler.is_body = true;
        body.accept(self)?;
        self.compiler.is_body = false;
        if self.methods.len() > 0 {
            if self.compiler.bytecode.len() > 0 {
                return Err(body.to_err("Can not use methods and operators at the same time in this task.").into())
            }
            let body = mem::take(&mut self.methods);
            let task_ids = mem::take(&mut self.method_ids);
            self.tasks.push(Task::Complex(ComplexTask{preconditions, cost:cost.unwrap_or(0), body, effects, task_ids}))
        } else {
            let body = mem::take(&mut self.compiler.bytecode);
            self.tasks.push(Task::Primitive(PrimitiveTask{preconditions, cost:cost.unwrap_or(0), body, effects}))
        }
        Ok(())
    }
    fn build_neighbor_map(&mut self) {
        for (i, x) in self.tasks.iter().enumerate() {
            let effects = x.get_state_effects();
            let mut to_vec = Vec::new();
            for (iy, y) in self.tasks.iter().enumerate() {
                if iy != i {
                    if effects.intersection(&y.get_state_depends()).count() > 0 {
                        to_vec.push(iy)
                    }
                }
            }
            self.neighbors.insert(i, to_vec);
        }
    }

    fn compile(&mut self, filepath:&str, is_include:bool) -> Result<(), Error> {
        let content = fs::read_to_string(filepath)?;
        let ast = Parser::parse(content.as_str())?;
        let current_pass_count = self.pass_count;
        self.pass_count = 0;
        ast.iter().try_for_each(|s| s.accept(self))?;
        self.pass_count = 1;
        ast.iter().try_for_each(|s| s.accept(self))?;
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
       
    pub fn from_file(filepath:&str, type_counts:HashMap<String, usize>) -> Result<Domain, Error> {   
        let mut domain = Domain{filepath:String::from(filepath), type_counts, tasks:Vec::new(), compiler:ExpressionCompiler::new(), methods:Vec::new(), method_ids:HashMap::new(), neighbors:HashMap::new(), pass_count:0, main_id:None};

        match domain.compile(filepath, false) {
            Ok(_) => {domain.build_neighbor_map(); Ok(domain)},
            Err(mut e) => {if !e.has_path() { e.set_path(filepath);} Err(e)} 
        }
    }
}