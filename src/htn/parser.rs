use std::{fmt::{self, Write}, rc::Rc, convert::TryInto, collections::{HashMap, btree_map::Values, HashSet}, ops::Deref};

use super::planner::State;

pub struct Error {
    line: usize,
    col: usize,
    message: String
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line:{} col:{} {}", self.line, self.col, self.message)
    }
}
impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line:{} col:{} {}", self.line, self.col, self.message)
    }
}

#[derive(Debug, Clone)]
pub enum TokenData {
    TASK,
    METHOD,
    ELSE,
    EFFECTS,
    COST,
    PASS,
    LABEL(String),
    LITERAL(i32),
    COMMA,
    EQUALS,
    EQUALS_EQUALS,
    MINUS,
    PLUS,
    SLASH,
    STAR,
    GREATER,
    SMALLER,
    GREATER_OR_EQUALS,
    SMALLER_OR_EQUALS,
    NOT_EQUALS,
    SUBTRACT_FROM,
    ADD_TO,
    MULTIPLY_BY,
    DIVIDE_BY,
    OR,
    AND,
    NOT,
    OPEN_PAREN,
    CLOSE_PAREN,
    COLON,
    BLOCK_START,
    BLOCK_END,
    STATEMENT_END,
    EOF

}

#[derive(Clone, Debug)]
pub struct Token {
    line: usize,
    col: usize,
    len: usize,
    pub t: TokenData,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.t)
    }
}

impl fmt::Display for TokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TASK => write!(f, "TASK"),
            Self::METHOD => write!(f, "METHOD"),
            Self::ELSE => write!(f, "ELSE"),
            Self::EFFECTS => write!(f, "EFFECTS"),
            Self::PASS => write!(f, "PASS"),
            Self::COST => write!(f, "COST"),
            Self::LABEL(l) => write!(f, "{}", l),
            Self::LITERAL(l) => write!(f, "'{}'", l),
            Self::COMMA => write!(f, ","),
            Self::EQUALS => write!(f, "="),
            Self::EQUALS_EQUALS => write!(f, "=="),
            Self::MINUS => write!(f, "-"),
            Self::PLUS => write!(f, "+"),
            Self::SLASH => write!(f, "/"),
            Self::STAR => write!(f, "*"),
            Self::GREATER => write!(f, ">"),
            Self::SMALLER => write!(f, "<"),
            Self::GREATER_OR_EQUALS => write!(f, ">="),
            Self::SMALLER_OR_EQUALS => write!(f, "<="),
            Self::NOT_EQUALS => write!(f, "!="),
            Self::SUBTRACT_FROM => write!(f, "-="),
            Self::ADD_TO => write!(f, "+="),
            Self::MULTIPLY_BY => write!(f, "*="),
            Self::DIVIDE_BY => write!(f, "/="),
            Self::OR => write!(f, "|"),
            Self::AND => write!(f, "&"),
            Self::NOT => write!(f, "!"),
            Self::OPEN_PAREN => write!(f, "("),
            Self::CLOSE_PAREN => write!(f, ")"),
            Self::COLON => write!(f, ":"),
            Self::BLOCK_START => write!(f, "{{"),
            Self::BLOCK_END => write!(f, "}}"),
            Self::STATEMENT_END => write!(f, ";"),
            Self::EOF => write!(f, "<<EOF"),
        }
    }
}

enum DepthSeparator {
    TABS,
    SPACES(usize)
}

pub struct Lexer {

}

pub struct Parser {
    idx: usize,
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn tokenize(htn_source: &str) -> Result<Vec<Token>, Error> {
        let mut line = 1; // current source line, used for error reporting by Tokens
        let mut col = 1; // current source column, used for error reporting by Tokens
        let mut last_block_tab_depth = 0; // needed to insert BLOCK_END tokens properly
        let mut tab_depth = 0; // For tabs, tab_depth just counts tabs at the beginning of line 
        // For spaces, tab_depth counts spaces first, then on the first non-whitespace character figures out 
        // how many spaces are equal to a new block, and divides amount of spaces by however many spaces constitute a new block
        let mut depth_separator : Option<DepthSeparator> = None;
        let mut is_comment = false; // ignores the rest of the line
        let mut is_newline = true; // stays true between \n and first non-whitespace characters to allow for tabs vs spaces detection
        let mut r = Vec::<Token>::new(); // result
        let mut it = htn_source.chars().peekable();
        while let Some(c) = it.next() {
            if is_comment && c != '\n' {
                continue;
            }
            match c {
                '#' => is_comment = true,
                ':' => r.push(Token{line, col, len:1, t:TokenData::COLON}),
                '(' => r.push(Token{line, col, len:1, t:TokenData::OPEN_PAREN}),
                ')' => r.push(Token{line, col, len:1, t:TokenData::CLOSE_PAREN}),
                '=' => r.push(Token{line, col, len:1, t:TokenData::EQUALS}),
                '-' => r.push(Token{line, col, len:1, t:TokenData::MINUS}),
                '+' => r.push(Token{line, col, len:1, t:TokenData::PLUS}),
                '/' => r.push(Token{line, col, len:1, t:TokenData::SLASH}),
                '*' => r.push(Token{line, col, len:1, t:TokenData::STAR}),
                '>' => r.push(Token{line, col, len:1, t:TokenData::GREATER}),
                '<' => r.push(Token{line, col, len:1, t:TokenData::SMALLER}),
                '!' => r.push(Token{line, col, len:1, t:TokenData::NOT}),
                '|' => r.push(Token{line, col, len:1, t:TokenData::OR}),
                '&' => r.push(Token{line, col, len:1, t:TokenData::AND}),
                ',' => r.push(Token{line, col, len:1, t:TokenData::COMMA}),
                '\n' => { 
                    is_comment = false; 
                    is_newline = true;
                    tab_depth = 0;
                    line += 1; 
                    col = 1; 
                },
                c if !c.is_whitespace() => {
                    if is_newline { 
                        // Now figure out tabs vs spaces and what tab_depth of current line is
                        // for block start/end detection
                        if let Some(DepthSeparator::SPACES(0)) = depth_separator { // we've seen spaces at the beginning of the line before, but it's the first ever word that's offset
                            depth_separator = Some(DepthSeparator::SPACES(tab_depth));
                            tab_depth = 1;
                        } else if let Some(DepthSeparator::SPACES(sc)) = depth_separator { // we already know that we're using spaces and how many
                            if tab_depth % sc != 0 {
                                return Err(Error{line, col, message:String::from("Unexpected amount of spaces.")});
                            }
                            tab_depth = tab_depth / sc;
                        } // else we're using tabs and '\t' => branch of this match counts tab_depth properly
                        if tab_depth > last_block_tab_depth { // new block start
                            if let Some(Token{t:TokenData::BLOCK_END,..}) = r.last() {
                                return Err(Error{line, col, message:String::from("Unexpected block identation.")});
                            }
                            r.push(Token{line, col:1, len:0, t:TokenData::BLOCK_START});
                            last_block_tab_depth = tab_depth;
                        } else if tab_depth == last_block_tab_depth && r.len() > 0 { // it's a new line for the old block. Previous statement has ended.
                            r.push(Token{line:line-1, col:r.last().unwrap().col+1, len:0, t:TokenData::STATEMENT_END});
                        } else {
                            while tab_depth < last_block_tab_depth {  // block(s) have ended
                                if let Some(Token{t:TokenData::BLOCK_END,..}) = r.last() {
                                    // if multiple blocks are ending, just keep adding BLOCK_END
                                } else {
                                    // last block has ended, so did the previous statement
                                    r.push(Token{line:line-1, col:r.last().unwrap().col+1, len:0, t:TokenData::STATEMENT_END});
                                }
                                r.push(Token{line, col:1, len:0, t:TokenData::BLOCK_END});
                                last_block_tab_depth -=1;
                            }
                        }
                        is_newline = false;
                    } // done with tab depth detection
                    // Now to see what kind of token this character will yield.
                    if let Some(mut last_token) = r.last_mut() {
                        if let Token{t:TokenData::LABEL(label), ..} = last_token {
                            label.push(c);
                            last_token.len += 1;
                            let should_convert = if let Some(next_char) = it.peek() { !next_char.is_alphanumeric() } else { true };
                            if let Some(t) = if should_convert {
                                    match label.clone().as_str() { // if the label composes a keyword, replace with keyword
                                        "task" => Some(TokenData::TASK),
                                        "method" => Some(TokenData::METHOD),
                                        "else" => Some(TokenData::ELSE),
                                        "effects" => Some(TokenData::EFFECTS),
                                        "pass" => Some(TokenData::PASS),
                                        "cost" => Some(TokenData::COST),
                                        "or" => Some(TokenData::OR),
                                        "and" => Some(TokenData::AND),
                                        "not" => Some(TokenData::NOT),
                                        "false" => Some(TokenData::LITERAL(0)),
                                        "true" => Some(TokenData::LITERAL(1)),
                                        _ => if let Ok(literal) = label.parse::<i32>() {
                                                Some(TokenData::LITERAL(literal))
                                            } else { None },
                                    }
                                } else { None } {
                                last_token.t = t;
                            }
                        } else { // last token isn't a label so we're starting a new label or keyword
                            if let Some(next_char) = it.peek() {
                                if !next_char.is_alphanumeric() {
                                    if let Some(digit) = c.to_digit(10) {
                                        r.push(Token{line, col, len:1, t:TokenData::LITERAL(digit.try_into().unwrap())})
                                    } else {
                                        r.push(Token{line, col, len:1, t:TokenData::LABEL(String::from(c))})
                                    }
                                } else {
                                    r.push(Token{line, col, len:1, t:TokenData::LABEL(String::from(c))})
                                }
                            } else {
                                r.push(Token{line, col, len:1, t:TokenData::LABEL(String::from(c))})
                            }
                            
                        }
                        
                        
                    } else {
                        r.push(Token{line, col, len:1, t:TokenData::LABEL(String::from(c))})
                    }
                },
                '\t' if is_newline => { if depth_separator.is_none() { // if we don't know if we're using tabs or spaces
                        depth_separator = Some(DepthSeparator::TABS);
                    } else if let Some(DepthSeparator::SPACES(_)) = depth_separator {
                        return Err(Error{line, col, message:String::from("Tabs and spaces can't be used together")})
                    }
                    tab_depth += 1;
                },
                c if c.is_whitespace() => {
                    if is_newline { 
                        if depth_separator.is_none() {
                            depth_separator = Some(DepthSeparator::SPACES(0));
                        } else if let Some(DepthSeparator::TABS) = depth_separator {
                            return Err(Error{line, col, message:String::from("Tabs and spaces can't be used together")})
                        }
                        tab_depth += 1; // tab_depth counts amount of spaces seen first. Then on the first non-whitespace character we convert amount of spaces to actual tab-depth
                    }
                    if if let Some(next_char) = it.peek() { next_char.is_alphanumeric() } else { false } {
                        r.push(Token{line, col, len:1, t:TokenData::LABEL(String::new())})
                    }
                },
                _ => (),
            }
            col += 1;
            if let Some(Token{t:TokenData::EQUALS,..}) = r.last() { // combine multi-symbol operands like -= and <=
                match r.get(r.len()-2) {
                    Some(Token{t:TokenData::EQUALS,..}) => {r.pop(); r.last_mut().unwrap().t = TokenData::EQUALS_EQUALS},
                    Some(Token{t:TokenData::SMALLER,..}) => {r.pop(); r.last_mut().unwrap().t = TokenData::SMALLER_OR_EQUALS},
                    Some(Token{t:TokenData::GREATER,..}) => {r.pop(); r.last_mut().unwrap().t = TokenData::GREATER_OR_EQUALS},
                    Some(Token{t:TokenData::NOT,..}) => {r.pop(); r.last_mut().unwrap().t = TokenData::NOT_EQUALS},
                    Some(Token{t:TokenData::MINUS,..}) => {r.pop(); r.last_mut().unwrap().t = TokenData::SUBTRACT_FROM},
                    Some(Token{t:TokenData::PLUS,..}) => {r.pop(); r.last_mut().unwrap().t = TokenData::ADD_TO},
                    Some(Token{t:TokenData::SLASH,..}) => {r.pop(); r.last_mut().unwrap().t = TokenData::DIVIDE_BY},
                    Some(Token{t:TokenData::STAR,..}) => {r.pop(); r.last_mut().unwrap().t = TokenData::MULTIPLY_BY},
                    _ => (),
                }
            }
        }
        // End of file. Finish off whatever blocks have been here
        // if last_block_tab_depth > 0 {
        if r.len() > 0 {
            r.push(Token{line, col, len:0, t:TokenData::STATEMENT_END});
        }
        while last_block_tab_depth > 0 {
            r.push(Token{line, col, len:0, t:TokenData::BLOCK_END});
            last_block_tab_depth -= 1;
            }
        // }
        r.push(Token{line, col, len:0, t:TokenData::EOF});
        Ok(r)
    }
}
#[derive(Clone, Debug)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>), // left Token right
    Grouping(Box<Expr>, Token), // e.g. '(' expression ')'
    Literal(i32, Token),
    Variable(Rc<String>, Token),
    Unary(Token, Box<Expr>), // Token right
    Assignment(Rc<String>, Box<Expr>, Token), // name, value
    Call(Rc<String>, Token, Vec<Expr>), // callee, closing parenthesis token, args
    Noop(Token)
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binary(left, op, right) => write!(f, "{} {} {}", left, op, right),
            Self::Grouping(g, _) => write!(f, "({})", g),
            Self::Literal(val, _) => write!(f, "{}", val),
            Self::Variable(var, _) => write!(f, "{}", var),
            Self::Unary(op, right) => write!(f, "{}{}", op, right),
            Self::Assignment(val, right, _) => write!(f, "{} = {}", val, right),
            Self::Call(func, _, args) => {
                write!(f, "{}(", func)?; 
                args.iter().take(1).try_for_each(|expr| write!(f, "{}", expr))?;
                args.iter().skip(1).try_for_each(|expr| write!(f, ", {}", expr))?; 
                write!(f, ")")
            },
            Self::Noop(_) => write!(f, "nop"),
        }
    }
}

impl Expr {
    pub fn eval(&self, state:&HashMap<Rc<String>, i32>) -> Result<i32, Error> {
        use TokenData::*;
        match self {
            Self::Binary(left, op, right) => {
                let left = left.eval(state)?;
                let right = right.eval(state)?;
                match op.t {
                    EQUALS_EQUALS => Ok((left == right) as i32),
                    MINUS => Ok(left - right),
                    PLUS => Ok(left + right),
                    SLASH => Ok(left / right),
                    STAR => Ok(left * right),
                    GREATER => Ok((left > right) as i32),
                    SMALLER => Ok((left < right) as i32),
                    GREATER_OR_EQUALS => Ok((left >= right) as i32),
                    SMALLER_OR_EQUALS => Ok((left <= right) as i32),
                    NOT_EQUALS => Ok((left != right) as i32),
                    OR => Ok(left | right),
                    AND => Ok(left & right),
                    _ => Err(Error{line:op.line, col:op.col, message:String::from("Unsupported operation.")}),
                }
            },
            Self::Grouping(g, _) => Ok(g.eval(state)?),
            Self::Literal(val, _) => Ok(*val),
            Self::Variable(var, tok) => if let Some(val) = state.get(var) {
                Ok(*val)
            } else {
                Err(Error{line:tok.line, col:tok.col, message:format!("Unknown variable name: {}.", var)})
            },
            Self::Unary(op, right) => if let NOT = op.t { if right.eval(state)? == 0 { Ok(1) } else { Ok(0) } } else { 
                Err(Error{line:op.line, col:op.col, message:String::from("Unexpected unary operator.")})
            },
            Expr::Call(_, tok, _) | 
            Expr::Assignment(_,_,tok) |
            Expr::Noop(tok) => Err(Error{line:tok.line, col:tok.col, message:String::from("Unable to evaluate this expression.")}),
            
        }
    }

    pub fn eval_mut(&self, state:&mut HashMap<Rc<String>, i32>) -> Result<i32, Error> {
        match self {
            Self::Assignment(var, right, _) => {let r = right.eval(state)?; state.insert(var.clone(), r); Ok(r)},
            _ => self.eval(state)
        }
    }

    pub fn world_variables(&self) -> HashSet<Rc<String>> {
        let mut vars = HashSet::new();
        match self {
            Self::Binary(left, _, right) => {vars.extend(left.world_variables()); vars.extend(right.world_variables()); },
            Self::Grouping(sub, _) |
            Self::Unary(_, sub) => vars.extend(sub.world_variables()),
            Self::Assignment(var, sub, _) => {vars.insert(var.clone()); vars.extend(sub.world_variables());},
            Self::Variable(var, _) => {vars.insert(var.clone());},
            Self::Literal(_, _) |
            Self::Call(_, _, _) |
            Self::Noop(_) => (),
        }
        vars
    }

    pub fn get_assignment_target(&self) -> Option<Rc<String>> {
        if let Self::Assignment(target, _,_) = self {
            Some(target.clone())
        } else {
            None
        }
    }

    pub fn get_call_target(&self) -> Option<Rc<String>> {
        match self {
            Self::Call(target, _,_) => Some(target.clone()),
            Self::Assignment(_,left,_) => left.get_call_target(),
            _ => None,
        }
    }

    pub fn get_call_arguments(&self) -> Vec<Rc<String>> {
        let mut result = Vec::new();    
        match self {
            Self::Call(_,_,arg_vec) => arg_vec.iter().for_each(|e| result.extend(e.world_variables().iter().map(|v| v.clone()))),
            Self::Assignment(_,left,_) => result.extend(left.get_call_arguments()),
            _ => (),
        }
        result
    }

    pub fn to_err(&self, msg:String) -> Error {
        match self {
            Self::Binary(_, tok, _) |
            Self::Grouping(_, tok) |
            Self::Literal(_, tok) |
            Self::Variable(_, tok) |
            Self::Unary(tok, _) |
            Self::Assignment(_, _, tok) |
            Self::Call(_, tok, _) |
            Self::Noop(tok) => Error{line:tok.line, col:tok.col, message:msg}
        }
    }

    pub fn line_no(&self) -> usize {
        match self {
            Self::Binary(_, tok, _) |
            Self::Grouping(_, tok) |
            Self::Literal(_, tok) |
            Self::Variable(_, tok) |
            Self::Unary(tok, _) |
            Self::Assignment(_, _, tok) |
            Self::Call(_, tok, _) |
            Self::Noop(tok) => tok.line
        }
    }
}

#[derive(Clone)]
pub enum Stmt {
    Method{
        name:Rc<String>, 
        preconditions:Option<Rc<Expr>>, 
        cost:Option<Expr>, 
        body:Box<Stmt>, 
        token:Token
    }, 
    Task{
        name:Rc<String>, 
        preconditions:Option<Rc<Expr>>, 
        cost: Option<Expr>,
        body:Box<Stmt>, 
        effects:Option<Box<Stmt>>, 
        token:Token
    }, 
    Block(Vec<Stmt>),
    Expression(Expr)
}

pub struct StmtFormatter<'a> {
    pub max_line_count: usize,
    pub depth: usize,
    pub stmt: &'a Stmt
}

impl std::fmt::Display for StmtFormatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let new_depth = self.depth + 2;
        match self.stmt {
            Stmt::Method{ name, preconditions:Some(preconditions), cost, body, token } => write!(f, "{:>lc$}: {:>depth$}{} {}({}) cost {}:{}", self.stmt.line_no(), ' ', token, name, preconditions, cost.as_ref().unwrap_or(&Expr::Noop(token.clone())), StmtFormatter{depth:new_depth, stmt:body, max_line_count:self.max_line_count}, depth=self.depth, lc=self.max_line_count),
            Stmt::Method{ name, preconditions:None, cost, body, token } => write!(f, "{:>lc$}: {:>depth$}{} {} cost {}:{}", self.stmt.line_no(), ' ', token, name, cost.as_ref().unwrap_or(&Expr::Noop(token.clone())), StmtFormatter{depth:new_depth, stmt:body, max_line_count:self.max_line_count}, depth=self.depth, lc=self.max_line_count),
            Stmt::Task{ name, preconditions:Some(preconditions), cost, body, effects:None, token } => write!(f, "{:>lc$}: {:>depth$}{} {}({}) cost {}:{}", self.stmt.line_no(), ' ',token, name, preconditions, cost.as_ref().unwrap_or(&Expr::Noop(token.clone())), StmtFormatter{depth:new_depth, stmt:body, max_line_count:self.max_line_count}, depth=self.depth, lc=self.max_line_count),
            Stmt::Task{ name, preconditions:None, cost, body, effects:None, token } => write!(f, "{:>lc$}: {:>depth$}{} {} cost {}:{}", self.stmt.line_no(), ' ',token, name, cost.as_ref().unwrap_or(&Expr::Noop(token.clone())), StmtFormatter{depth:new_depth, stmt:body, max_line_count:self.max_line_count}, depth=self.depth, lc=self.max_line_count),
            Stmt::Task{ name, preconditions:Some(preconditions), cost, body, effects:Some(effects), token } => write!(f, "{:>lc$}: {:>depth$}{} {}({}) cost {}:{}{:>lc$}: {:>depth$}EFFECTS:{}", self.stmt.line_no(), ' ',token, name, preconditions, cost.as_ref().unwrap_or(&Expr::Noop(token.clone())), StmtFormatter{depth:new_depth, stmt:body, max_line_count:self.max_line_count}, effects.line_no(), ' ', StmtFormatter{depth:new_depth, stmt:effects, max_line_count:self.max_line_count}, depth=self.depth, lc=self.max_line_count),
            Stmt::Task{ name, preconditions:None, cost, body, effects:Some(effects), token } => write!(f, "{:>lc$}: {:>depth$}{} {} cost {}:{}{:>lc$}: {:>depth$}EFFECTS:{}", self.stmt.line_no(), ' ',token, name, cost.as_ref().unwrap_or(&Expr::Noop(token.clone())), StmtFormatter{depth:new_depth, stmt:body, max_line_count:self.max_line_count}, effects.line_no(), ' ', StmtFormatter{depth:new_depth, stmt:effects, max_line_count:self.max_line_count}, depth=self.depth, lc=self.max_line_count),
            Stmt::Block(blk) => {
                
                write!(f, "\n")?;
                blk.iter().try_for_each(|stmt| write!(f, "{}", StmtFormatter{depth:new_depth, stmt, max_line_count:self.max_line_count}))
                // write!(f, "block_end\n")
            },
            Stmt::Expression(e) => write!(f, "{:>lc$}: {:>depth$}{}\n", self.stmt.line_no(), ' ', e, depth=self.depth, lc=self.max_line_count),
        }
    }
}

impl std::fmt::Debug for StmtFormatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)?;
        writeln!(f, "Effects: {:?}", self.stmt.affects())?;
        writeln!(f, "Depends {:?}", self.stmt.depends())
    }
}


impl Stmt {
    pub fn to_err(&self, msg:String) -> Error {
        match self {
            Stmt::Method{token,..} |
            Stmt::Task{token,..} => Error{line:token.line, col:token.col, message:msg},
            Stmt::Block(blk) => blk.first().expect("Unable to generate error for empty block.").to_err(msg),
            Stmt::Expression(e) => e.to_err(msg),
        }
    }

    pub fn line_no(&self) -> usize {
        match self {
            Self::Method{token,..} |
            Self::Task{token,..} => token.line,
            Stmt::Block(blk) => blk.first().expect("Unable to get line for empty block").line_no(),
            Stmt::Expression(e) => e.line_no()
        }
    }
    pub fn name(&self) -> Result<Rc<String>, Error> {
        match self {
            Self::Method{name, ..} |
            Self::Task{name, ..} => Ok(name.clone()),
            _ => Err(self.to_err(String::from("Statement is not a Task or a Method.")))
        }
    }

    pub fn cost(&self) -> Result<Option<i32>, Error> {
        match self {
            Self::Method{cost:Some(cost),..} | 
            Self::Task{cost:Some(cost),..} => Ok(Some(cost.eval(&HashMap::new())?)),
            Self::Method{cost:None,..} | 
            Self::Task{cost:None,..} => Ok(None),
            _ => Err(self.to_err(String::from("Statement is not a Task or a Method.")))
        }
    }

    pub fn affects(&self) -> HashSet<Rc<String>> {
        let mut result = HashSet::new();
        match self {
            Self::Task{effects:Some(effects),..} => result.extend(effects.affects()),
            Self::Block(blk) => blk.iter().for_each(|stmt| result.extend(stmt.affects())),
            Self::Expression(e) => result.extend(e.world_variables()),
            Self::Method{..} | Self::Task{..} => (),
        }
        result
    }

    pub fn depends(&self) -> HashSet<Rc<String>> {
        let mut result = HashSet::new();
        match self {
            Stmt::Method{preconditions:Some(preconditions), ..} |
            Stmt::Task{preconditions:Some(preconditions),..} => result.extend(preconditions.world_variables()),
            Stmt::Block(blk) => blk.iter().for_each(|stmt| result.extend(stmt.depends())),
            _ => (),
        }
        result
    }

    pub fn is_composite(&self) -> Result<bool, Error> {
        match self {
            Self::Task{body,..} |
            Self::Method{body,..} => body.is_composite(),
            Self::Block(blk) => Ok(blk.iter().all(|stmt| match stmt {
                Self::Method{..} => true,
                _ => false,
            })),
            _ => Err(self.to_err(String::from("Unable to decide composite/primitive")))
        }
    }

    pub fn for_each_method<'a, F>(&'a self, func:&mut F) -> Result<(), Error> where F: FnMut(&'a Self) -> Result<(), Error> {
        match self {
            Self::Task{body,..} => body.for_each_method(func),
            Self::Block(blk) => {for stmt in blk { stmt.for_each_method(func)? } Ok(())},
            Self::Method{..} => func(self),
            _ => Ok(()),
        }
    }

    pub fn for_each_operator<'a, F>(&'a self, func:&mut F) -> Result<bool, Error> where F: FnMut(&'a Expr) -> Result<bool, Error> {
        match self {
            Self::Task{body,..} |
            Self::Method{body,..} => body.for_each_operator(func),
            Self::Block(blk) => {let mut acc = true; for stmt in blk { acc &= stmt.for_each_operator(func)? } Ok(acc)},
            Self::Expression(e) => func(e)
        }
    }
 
    pub fn for_each_method_while<'a, F>(&'a self, func:&mut F) -> Result<bool, Error> where F: FnMut(&'a Self) -> Result<bool, Error> {
        match self {
            Self::Task{body,..} => body.for_each_method_while(func),
            Self::Block(blk) => {for stmt in blk { if !stmt.for_each_method_while(func)? { break } }; Ok(false)},
            Self::Method{..} => func(self),
            _ => Ok(false),
        }
    }

    pub fn preconditions(&self) -> Result<Option<Rc<Expr>>, Error> {
        match self {
            Self::Method{preconditions, ..} |
            Self::Task{preconditions, ..} => Ok(preconditions.clone()),
            _ => Err(self.to_err(String::from("Statement is not a Task or a Method.")))
        }
    }

    pub fn are_preconditions_satisfied(&self, state:&HashMap<Rc<String>, i32>) -> Result<i32, Error> {
        match &self {
            Self::Method{preconditions:Some(preconditions),..} |
            Self::Task{preconditions:Some(preconditions),..} => preconditions.eval(state),
            _ => Ok(1)
        }
    }

    pub fn effects(&self) -> Result<&Option<Box<Stmt>>, Error> {
        match &self {
            Self::Task{effects,..} => Ok(effects),
            _ => Err(self.to_err(String::from("Only tasks have effects")))
        }
    }

    pub fn effect(&self, state:&mut State) -> i32 {
        match &self {
            Self::Task{effects:Some(effects),..} => effects.effect(state),
            Self::Block(blk) => { for stmt in blk { stmt.effect(state); } 1},
            Self::Expression(e) => e.eval_mut(&mut state.0).unwrap(),
            _ => 0,
        }
    }
}

macro_rules! perror {
    ($s:expr, $o:literal, $e:literal) => { 
        {let line = $s.tokens[$s.idx-$o].line;
        let col = $s.tokens[$s.idx-$o].col;
        Err(Error{line, col, message:String::from($e)})}
    }
}

macro_rules! pexpect {
    ($s:expr, $p:pat, $do:block, $e:literal) => {
        if let $p = &$s.tokens[$s.idx].t {
            $s.idx += 1;
            $do
        } else {
            perror!($s, 0, $e)
        }
    }
}

macro_rules! pexpect_prevtoken {
    ($s:expr, $p:pat, $do:block, $e:literal) => {
        if let $p = &$s.tokens[$s.idx].t {
            $s.idx += 1;
            $do
        } else {
            perror!($s, 1, $e)
        }
    }
}

macro_rules! pmatch {
    ($s:expr, $p:pat, $do:block) => {
        if let $p = &$s.tokens[$s.idx].t {
            $s.idx += 1;
            $do
        } 
    }
}

macro_rules! ptest {
    ($s:expr, $p:pat, $do_match:block else $do_nomatch:block) => {
            if let $p = &$s.tokens[$s.idx].t {
                $s.idx += 1;
                $do_match
            } else {
                $do_nomatch
            }
    }
}

impl Parser {
    fn error_recover(&mut self) {
        self.idx += 1;
        while self.idx + 1 < self.tokens.len() {
            if let TokenData::TASK = self.tokens[self.idx].t {
                return;
            }
            self.idx += 1;
        }
    }
    fn primary(&mut self) -> Result<Expr, Error> {
        // Literal | "(" expression ")"
        if let TokenData::LITERAL(val)  = self.tokens[self.idx].t {
            self.idx += 1;
            Ok(Expr::Literal(val, self.tokens[self.idx].clone()))
        } else if let TokenData::OPEN_PAREN = self.tokens[self.idx].t {
            self.idx += 1;
            let expr = self.expression()?;
            pexpect!(self, TokenData::CLOSE_PAREN, {
                Ok(Expr::Grouping(Box::new(expr), self.tokens[self.idx].clone()))
            }, "Expected ')' after expression.")
        } else if let TokenData::LABEL(name) = &self.tokens[self.idx].t {
            self.idx += 1;
            Ok(Expr::Variable(Rc::new(name.clone()), self.tokens[self.idx-1].clone()))
        } else if let TokenData::PASS = &self.tokens[self.idx].t {
            self.idx += 1;
            Ok(Expr::Noop(self.tokens[self.idx-1].clone()))
        } else {
            perror!(self, 0, "Expected expression.")
        }

    }
    fn call(&mut self) -> Result<Expr, Error> {
        let expr = self.primary()?;
        ptest!(self, TokenData::OPEN_PAREN, {
            let mut args = Vec::<Expr>::new();
            loop { 
                ptest!(self, TokenData::CLOSE_PAREN, {
                    break if let Expr::Variable(name, _) = expr {
                        Ok(Expr::Call(name, self.tokens[self.idx-1].clone(), args))
                    } else {
                        perror!(self, 1, "Expected function name before call.")
                    }
                } else {
                    args.push(self.expression()?);
                    ptest!(self, TokenData::COMMA, {} else {});
                })
            }
        } else {
            Ok(expr)
        })
    }
    fn unary(&mut self) -> Result<Expr, Error> {
        ptest!(self, (TokenData::NOT | TokenData::MINUS), {
            let operator = self.tokens[self.idx-1].clone();
            let right = self.unary()?;
            Ok(Expr::Unary(operator, Box::new(right)))
        } else {
            self.call()
        }) 
    }
    fn factor(&mut self) -> Result<Expr, Error> {
        let mut expr = self.unary()?;
        while let TokenData::SLASH | TokenData::STAR | TokenData::AND = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn term(&mut self) -> Result<Expr, Error> {
        let mut expr = self.factor()?;
        while let TokenData::MINUS | TokenData::PLUS | TokenData::OR = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn comparison(&mut self) -> Result<Expr, Error> {
        let mut expr = self.term()?;
        while let TokenData::GREATER | TokenData::GREATER_OR_EQUALS | TokenData::SMALLER | TokenData::SMALLER_OR_EQUALS = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn equality(&mut self) -> Result<Expr, Error> {
        let mut expr = self.comparison()?;
        while let TokenData::NOT_EQUALS | TokenData::EQUALS_EQUALS = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn assignment(&mut self) -> Result<Expr, Error> {
        let target = self.equality()?;
        ptest!(self, (TokenData::EQUALS 
            | TokenData::ADD_TO 
            | TokenData::SUBTRACT_FROM
            | TokenData::DIVIDE_BY
            | TokenData::MULTIPLY_BY), {
                if let Expr::Variable(varname, _) = &target {
                    let line = self.tokens[self.idx-1].line;
                    let col = self.tokens[self.idx-1].col;
                    let value_expr = match self.tokens[self.idx-1].t {
                        // TokenData::EQUALS => self.expression()?,
                        TokenData::ADD_TO => Expr::Binary(Box::new(Expr::Variable(varname.clone(), self.tokens[self.idx].clone())), Token{line, col, len:0, t:TokenData::PLUS}, Box::new(self.expression()?)),
                        TokenData::SUBTRACT_FROM => Expr::Binary(Box::new(Expr::Variable(varname.clone(), self.tokens[self.idx].clone())), Token{line, col, len:0, t:TokenData::MINUS}, Box::new(self.expression()?)),
                        TokenData::MULTIPLY_BY => Expr::Binary(Box::new(Expr::Variable(varname.clone(), self.tokens[self.idx].clone())), Token{line, col, len:0, t:TokenData::STAR}, Box::new(self.expression()?)),
                        TokenData::DIVIDE_BY => Expr::Binary(Box::new(Expr::Variable(varname.clone(), self.tokens[self.idx].clone())), Token{line, col, len:0, t:TokenData::SLASH}, Box::new(self.expression()?)),
                        _ => self.expression()?,
                    };
                    Ok(Expr::Assignment(varname.clone(), Box::new(value_expr), self.tokens[self.idx].clone()))
                } else {
                    let line = self.tokens[self.idx].line;
                    let col = self.tokens[self.idx].col;
                    Err(Error{line, col, message:String::from("Invalid assignment target.")})
                }
        } else {
            Ok(target)
        })
    }
    fn expression(&mut self) -> Result<Expr, Error> {
        self.assignment()
    }

    fn effects_statement(&mut self) -> Result<Stmt, Error> {
        pexpect_prevtoken!(self, TokenData::COLON, {self.statement(&None)}, "Expected ':' after 'effects'.")
    }
    fn task_statement(&mut self) -> Result<Stmt, Error> {
        let token_id = self.idx-1;
        pexpect!(self, TokenData::LABEL(name), {
            let name = Rc::new(name.clone());
            let mut preconditions = None;
            pmatch!(self, TokenData::OPEN_PAREN, {
                preconditions = Some(Rc::new(self.expression()?));
                pexpect!(self, TokenData::CLOSE_PAREN, {Ok(())}, "Expected ')' after task conditions.")?
            });
            let mut cost = None;
            pmatch!(self, TokenData::COST, {
                cost = Some(self.expression()?);
            });
            pexpect_prevtoken!(self, TokenData::COLON, {
                let body = Box::new(self.statement(&Some(name.clone()))?);
                let mut effects = None;
                pmatch!(self, TokenData::EFFECTS, {effects = Some(Box::new(self.effects_statement()?));});
                Ok(Stmt::Task{name, preconditions, cost, body, effects, token:self.tokens[token_id].clone()})
            }, "Expected ':' after task label.")
        }, "Expected label after 'task'.")
        
    }
    fn expression_statement(&mut self) -> Result<Stmt, Error> {
        let expr = self.expression()?;
        pexpect!(self, 
            (TokenData::STATEMENT_END | TokenData::BLOCK_START | TokenData::BLOCK_END), 
            {Ok(Stmt::Expression(expr))}, 
            "Expected new line after expression.")
    }
    fn block_statement(&mut self, parent_task:&Option<Rc<String>>) -> Result<Stmt, Error> {
        let mut stmts = Vec::<Stmt>::new();
        loop {
            let mut stmt = self.statement(parent_task)?;
            if let Stmt::Block(ref mut inner) = stmt {
                stmts.append(inner);
            } else {
                stmts.push(stmt);
            }

            if let TokenData::BLOCK_END | TokenData::EOF = self.tokens[self.idx].t {
                self.idx += 1;
                return Ok(Stmt::Block(stmts))
            } 
        }
        
    }
    fn method_statement(&mut self, parent_task:&Option<Rc<String>>) -> Result<Stmt, Error> {
        let token_id = self.idx-1;
        let parent_task = parent_task.as_ref().map(|p| p.clone()).unwrap();
        pexpect!(self, TokenData::LABEL(name), {
            let name = name.clone();
            let method_name = Rc::new(format!("{}.{}", parent_task, name));
            let mut preconditions: Option<Rc<Expr>> = None;
            pmatch!(self, TokenData::OPEN_PAREN, {
                preconditions = Some(Rc::new(self.expression()?));
                pexpect!(self, TokenData::CLOSE_PAREN, {Ok(())}, "Expected ')' after method conditions.'")?;
            });
            let mut cost = None;
            pmatch!(self, TokenData::COST, {
                cost = Some(self.expression()?);
            });
            pexpect_prevtoken!(self, TokenData::COLON, {
                let parent_statemet = Stmt::Method{name:method_name, preconditions:preconditions.as_ref().and_then(|p| Some(p.clone())), cost, body:Box::new(self.statement(&None)?), token:self.tokens[token_id].clone()};
                if preconditions.is_some() {
                    ptest!(self, TokenData::ELSE, {
                        let mut elsecost = None;
                        pmatch!(self, TokenData::COST, {
                            elsecost = Some(self.expression()?);
                        });
                        let token_id = self.idx-1;
                        pexpect!(self, TokenData::COLON, {
                            let else_conditions = Expr::Unary(Token{line:self.tokens[token_id].line, col:self.tokens[token_id].col, len:0, t:TokenData::NOT}, Box::new(preconditions.unwrap().deref().clone()));
                            let else_statement = Stmt::Method{name:Rc::new(format!("{}.Dont{}", parent_task, name)), preconditions:Some(Rc::new(else_conditions)), cost:elsecost, body:Box::new(self.statement(&None)?), token:self.tokens[token_id].clone()};
                            Ok(Stmt::Block(vec![parent_statemet, else_statement]))
                        }, "Expected ':' after else clause.")
                    } else {
                        Ok(parent_statemet)
                    })
                } else {
                    Ok(parent_statemet)
                }
            }, "Expected ':' after method declaration.")
        }, "Expected method name.")
    }
    fn statement(&mut self, parent:&Option<Rc<String>>) -> Result<Stmt, Error> {
        // println!("When Parsing a new statement, next token is {}.", self.tokens[self.idx]);
        let r = if let TokenData::TASK = self.tokens[self.idx].t {
            self.idx += 1;
            self.task_statement()
        } else if let TokenData::BLOCK_START = self.tokens[self.idx].t {
            self.idx += 1;
            self.block_statement(parent)
        } else if let TokenData::METHOD = self.tokens[self.idx].t {
            self.idx += 1;
            self.method_statement(parent)
        } else if let TokenData::EOF = self.tokens[self.idx].t {
            let line = self.tokens[self.idx].line;
            let col = self.tokens[self.idx].col;
            Err(Error{line, col, message:String::from("Reached the end of file")})
        } else {
            self.expression_statement()
        };
        // println!("After parsing a statement, next token is {}.", self.tokens[self.idx]);
        r
    }
    fn print_tokens(tokens: &Vec<Token>) {
        let mut depth = 0;
        println!("{}", tokens.iter().fold(String::new(), |acc, item| {
                let item_string = match item.t {
                    TokenData::BLOCK_START => {depth += 1; format!("{}", item)},
                    TokenData::BLOCK_END => {format!("{}\n", item)},
                    TokenData::COLON => format!("{}\n", item),
                    _ => format!("{} ", item),
                };
                let r = if acc.ends_with('\n') || acc.len() == 0 {
                    acc + &format!("{:4}: {:width$}{}", item.line, ' ', item_string, width=depth*4)
                } else if acc.ends_with('{') {
                    acc + &format!(" {}", item_string)
                } else {
                    acc + &format!("{}", item_string)
                };
                if let TokenData::BLOCK_END = item.t {
                    depth -= 1;
                };
                r
            }
        ));
    }
    pub fn print_parse_errors(e: &Error, htn_source:&str, filepath:&str) {
        let mut lines = htn_source.lines();
        let mut last_error_line = 0;
        if let Some(eline) = lines.nth(e.line - last_error_line-1) {
            let line_number_string = format!("{}", e.line);
            eprintln!("{}:{} Error:", filepath, e.line); 
            eprintln!("\t{}: {}", line_number_string, eline);
            last_error_line = e.line;
            let debug_str_col_pos = line_number_string.len() + 1 + e.col;
            eprintln!("\t{:->width$} {}\n",'^', e.message, width=debug_str_col_pos); 
        }
    }
    pub fn parse(htn_source: &str) -> Result<Vec<Stmt>, Vec<Error>> {
        let mut ast = Vec::<Stmt>::new();
        let mut errors = Vec::<Error>::new();
        let mut parser = match Lexer::tokenize(htn_source) {
            Ok(tokens) => Parser{idx:0, tokens},
            Err(e) => { errors.push(e); return Err(errors); }
        };
        // Parser::print_tokens(&parser.tokens);
        while parser.idx + 1 < parser.tokens.len() {
            match parser.statement(&None) {
                Err(e) => {errors.push(e); parser.error_recover(); },
                Ok(s) => ast.push(s)
            }
        }
        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(ast)
        }
    }
}