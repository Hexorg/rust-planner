use std::{fmt, rc::Rc, collections::{HashSet, HashMap}, ops::Deref, convert::{TryFrom, TryInto}};

pub struct Error {
    line: usize,
    col: usize,
    message: String,
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
impl std::error::Error for Error { }

#[derive(Clone, Debug)]
pub enum Literal {
    I(i32),
    F(f32),
    B(bool),
    S(String)
}


impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Literal::*;
        match self {
            I(i) => write!(f, "{}", i),
            F(fv) => write!(f, "{}", fv),
            B(b) => write!(f, "{}", if *b {"true"} else {"false"}),
            S(s) => write!(f, "\"{}\"", s)
        }
    }
}

#[derive(Debug, Clone)]
pub enum TokenData {
    Task,
    Method,
    Else,
    Effects,
    Cost,
    Pass,
    Label(String),
    Literal(Literal),
    Include,
    Comma,
    Equals,
    EqualsEquals,
    Minus,
    Plus,
    Slash,
    Star,
    Greater,
    Smaller,
    GreaterOrEquals,
    SmallerOrEquals,
    NotEquals,
    SubtractFrom,
    AddTo,
    MultiplyBy,
    DivideBy,
    Or,
    And,
    Not,
    OpenParenthesis,
    CloseParenthesis,
    Colon,
    BlockStart,
    BlockEnd,
    StatementEnd,
    EOF

}

#[derive(Clone, Debug)]
pub struct Token {
    pub line: usize,
    pub col: usize,
    pub len: usize,
    pub t: TokenData,
}
#[derive(Clone)]
pub struct LabelToken {
    pub line: usize,
    pub col: usize,
    pub len: usize,
    pub string: String,
    pub idx: Option<usize>
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.t)
    }
}

impl TryFrom<Token> for LabelToken {
    type Error = self::Error;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        if let TokenData::Label(name) = value.t {
            Ok(Self{line:value.line, col:value.col, len:value.len, string:name, idx:None})
        } else {
            Err(self::Error{line:value.line, col:value.col, message:String::from("Can't convert non-Label Token to LabelToken")})
        }
    }
}

impl fmt::Display for LabelToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.string)
    }
}

impl fmt::Display for TokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenData::*;
        match self {
            Task => write!(f, "TASK"),
            Method => write!(f, "METHOD"),
            Else => write!(f, "ELSE"),
            Effects => write!(f, "EFFECTS"),
            Pass => write!(f, "PASS"),
            Cost => write!(f, "COST"),
            Include => write!(f, "INCLUDE"),
            Label(l) => write!(f, "{}", l),
            Literal(l) => write!(f, "'{}'", l),
            Comma => write!(f, ","),
            Equals => write!(f, "="),
            EqualsEquals => write!(f, "=="),
            Minus => write!(f, "-"),
            Plus => write!(f, "+"),
            Slash => write!(f, "/"),
            Star => write!(f, "*"),
            Greater => write!(f, ">"),
            Smaller => write!(f, "<"),
            GreaterOrEquals => write!(f, ">="),
            SmallerOrEquals => write!(f, "<="),
            NotEquals => write!(f, "!="),
            SubtractFrom => write!(f, "-="),
            AddTo => write!(f, "+="),
            MultiplyBy => write!(f, "*="),
            DivideBy => write!(f, "/="),
            Or => write!(f, "|"),
            And => write!(f, "&"),
            Not => write!(f, "!"),
            OpenParenthesis => write!(f, "("),
            CloseParenthesis => write!(f, ")"),
            Colon => write!(f, ":"),
            BlockStart => write!(f, "{{"),
            BlockEnd => write!(f, "}}"),
            StatementEnd => write!(f, ";"),
            EOF => write!(f, "<<EOF"),
        }
    }
}

enum DepthSeparator {
    TABS,
    SPACES(usize)
}

pub struct Lexer { }

pub struct Parser {
    idx: usize,
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn tokenize(htn_source: &str) -> Result<Vec<Token>, Error> {
        use TokenData::*;
        let mut line = 1; // current source line, used for error reporting by Tokens
        let mut col = 1; // current source column, used for error reporting by Tokens
        let mut last_block_tab_depth = 0; // needed to insert BLOCK_END tokens properly
        let mut tab_depth = 0; // For tabs, tab_depth just counts tabs at the beginning of line 
        // For spaces, tab_depth counts spaces first, then on the first non-whitespace character figures out 
        // how many spaces are equal to a new block, and divides amount of spaces by however many spaces constitute a new block
        let mut depth_separator : Option<DepthSeparator> = None;
        let mut is_comment = false; // ignores the rest of the line
        let mut is_newline = true; // stays true between \n and first non-whitespace characters to allow for tabs vs spaces detection
        let mut string_data: Option<String> = None;
        let mut r = Vec::<Token>::new(); // result
        let mut it = htn_source.chars().peekable();
        while let Some(c) = it.next() {
            if is_comment && c != '\n' {
                continue;
            }
            if let Some(ref mut s) = string_data { 
                if c == '"' { 
                    r.push(Token{line, col, len:s.len(), t:Literal(self::Literal::S(s.to_string()))});
                    string_data = None;
                } else {
                    s.push(c);
                }
            } else { 
                match c {
                    '#' => is_comment = true,
                    ':' => r.push(Token{line, col, len:1, t:Colon}),
                    '(' => r.push(Token{line, col, len:1, t:OpenParenthesis}),
                    ')' => r.push(Token{line, col, len:1, t:CloseParenthesis}),
                    '=' => r.push(Token{line, col, len:1, t:Equals}),
                    '-' => r.push(Token{line, col, len:1, t:Minus}),
                    '+' => r.push(Token{line, col, len:1, t:Plus}),
                    '/' => r.push(Token{line, col, len:1, t:Slash}),
                    '*' => r.push(Token{line, col, len:1, t:Star}),
                    '>' => r.push(Token{line, col, len:1, t:Greater}),
                    '<' => r.push(Token{line, col, len:1, t:Smaller}),
                    '!' => r.push(Token{line, col, len:1, t:Not}),
                    '|' => r.push(Token{line, col, len:1, t:Or}),
                    '&' => r.push(Token{line, col, len:1, t:And}),
                    ',' => r.push(Token{line, col, len:1, t:Comma}),
                    '"' => string_data = Some(String::new()),
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
                                if let Some(Token{t:BlockEnd,..}) = r.last() {
                                    return Err(Error{line, col, message:String::from("Unexpected block identation.")});
                                }
                                r.push(Token{line, col:1, len:0, t:BlockStart});
                                last_block_tab_depth = tab_depth;
                            } else if tab_depth == last_block_tab_depth && r.len() > 0 { // it's a new line for the old block. Previous statement has ended.
                                r.push(Token{line:line-1, col:r.last().unwrap().col+1, len:0, t:StatementEnd});
                            } else {
                                while tab_depth < last_block_tab_depth {  // block(s) have ended
                                    if let Some(Token{t:BlockEnd,..}) = r.last() {
                                        // if multiple blocks are ending, just keep adding BLOCK_END
                                    } else {
                                        // last block has ended, so did the previous statement
                                        r.push(Token{line:line-1, col:r.last().unwrap().col+1, len:0, t:StatementEnd});
                                    }
                                    r.push(Token{line, col:1, len:0, t:BlockEnd});
                                    last_block_tab_depth -=1;
                                }
                            }
                            is_newline = false;
                        } // done with tab depth detection
                        // Now to see what kind of token this character will yield.
                        if let Some(mut last_token) = r.last_mut() {
                            let should_convert = if let Some(next_char) = it.peek() { !next_char.is_alphanumeric() && *next_char != '.' } else { true };
                            if let Token{t:Label(label), ..} = last_token {
                                label.push(c);
                                last_token.len += 1;
                                if let Some(t) = if should_convert {
                                        match label.as_str() { // if the label composes a keyword, replace with keyword
                                            "task" => Some(Task),
                                            "method" => Some(Method),
                                            "else" => Some(Else),
                                            "effects" => Some(Effects),
                                            "include" => Some(Include),
                                            "pass" => Some(Pass),
                                            "cost" => Some(Cost),
                                            "or" => Some(Or),
                                            "and" => Some(And),
                                            "not" => Some(Not),
                                            "false" => Some(Literal(self::Literal::B(false))),
                                            "true" => Some(Literal(self::Literal::B(true))),
                                            _ => { if label.contains('.') { 
                                                    if let Ok(literal) = label.parse::<f32>() {
                                                        Some(Literal(self::Literal::F(literal)))
                                                    } else {
                                                        None
                                                    }
                                                } else if let Ok(literal) = label.parse::<i32>() {
                                                    Some(Literal(self::Literal::I(literal)))
                                                } else { None }},
                                        }
                                    } else { None } {
                                    last_token.t = t;
                                }
                            } else { // last token isn't a label so we're starting a new label or keyword
                                if should_convert {
                                    if c.is_numeric() {
                                        r.push(Token{line, col, len:1, t:Literal(self::Literal::I(c.to_digit(10).unwrap() as i32))})
                                    }
                                } else {
                                    r.push(Token{line, col, len:1, t:Label(String::from(c))})
                                }
                            }
                        } else {
                            // this is the first token ever
                            r.push(Token{line, col, len:1, t:Label(String::from(c))})
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
                            r.push(Token{line, col, len:1, t:Label(String::new())})
                        }
                    },
                    _ => (),
                }
                if let Some(Token{t:Equals,..}) = r.last() { // combine multi-symbol operands like -= and <=
                    match r.get(r.len()-2) {
                        Some(Token{t:Equals,..}) => {r.pop(); r.last_mut().unwrap().t = EqualsEquals},
                        Some(Token{t:Smaller,..}) => {r.pop(); r.last_mut().unwrap().t = SmallerOrEquals},
                        Some(Token{t:Greater,..}) => {r.pop(); r.last_mut().unwrap().t = GreaterOrEquals},
                        Some(Token{t:Not,..}) => {r.pop(); r.last_mut().unwrap().t = NotEquals},
                        Some(Token{t:Minus,..}) => {r.pop(); r.last_mut().unwrap().t = SubtractFrom},
                        Some(Token{t:Plus,..}) => {r.pop(); r.last_mut().unwrap().t = AddTo},
                        Some(Token{t:Slash,..}) => {r.pop(); r.last_mut().unwrap().t = DivideBy},
                        Some(Token{t:Star,..}) => {r.pop(); r.last_mut().unwrap().t = MultiplyBy},
                        _ => (),
                    }
                }
            }
            col += 1;
            
        }
        // End of file. Finish off whatever blocks have been here
        // if last_block_tab_depth > 0 {
        if r.len() > 0 {
            r.push(Token{line, col, len:0, t:StatementEnd});
        }
        while last_block_tab_depth > 0 {
            r.push(Token{line, col, len:0, t:BlockEnd});
            last_block_tab_depth -= 1;
            }
        // }
        r.push(Token{line, col, len:0, t:EOF});
        Ok(r)
    }
}


#[derive(Clone)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>), // left Token right
    Grouping(Box<Expr>, Token), // e.g. '(' expression ')'
    Literal(Literal, Token),
    Variable(LabelToken),
    Unary(Token, Box<Expr>), // Token right
    Assignment(LabelToken, Box<Expr>), // name, value
    Call(LabelToken, Vec<Expr>), // callee, args
    Noop(Token)
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binary(left, op, right) => write!(f, "{} {} {}", left, op, right),
            Self::Grouping(g, _) => write!(f, "({})", g),
            Self::Literal(val, _) => write!(f, "{}", val),
            Self::Variable(var) => write!(f, "{}", var),
            Self::Unary(op, right) => write!(f, "{}{}", op, right),
            Self::Assignment(val, right) => write!(f, "{} = {}", val, right),
            Self::Call(func, args) => {
                write!(f, "{}(", func)?; 
                let mut i = args.iter();
                i.by_ref().take(1).try_for_each(|expr| write!(f, "{}", expr))?;
                i.try_for_each(|expr| write!(f, ", {}", expr))?; 
                write!(f, ")")
            },
            Self::Noop(_) => write!(f, "nop"),
        }
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binary(left, op, right) => write!(f, "({:?}){}({:?})", left, op, right),
            Self::Grouping(g, _) => write!(f, "({:?})", g),
            Self::Literal(val, _) => write!(f, ">{}<", val),
            Self::Variable(var) => write!(f, "var_{}", var),
            Self::Unary(op, right) => write!(f, "{}({:?})", op, right),
            Self::Assignment(val, right) => write!(f, "{} = {:?}", val, right),
            Self::Call(func, args) => {
                write!(f, "{}(", func)?; 
                let mut i = args.iter();
                i.by_ref().take(1).try_for_each(|expr| write!(f, "{:?}", expr))?;
                i.try_for_each(|expr| write!(f, ", {:?}", expr))?; 
                write!(f, ")")
            },
            Self::Noop(_) => write!(f, "nop"),
        }
    }
}


impl Expr {
    pub fn get_reads(&self) -> HashSet<&str> {
        let mut vars = HashSet::new();
        match self {
            Self::Binary(left, _, right) => {vars.extend(left.get_reads()); vars.extend(right.get_reads()); },
            Self::Grouping(sub, _) |
            Self::Unary(_, sub) |
            Self::Assignment(_, sub)=> vars.extend(sub.get_reads()),
            Self::Variable(var) => {vars.insert(var.string.as_str());},
            Self::Literal(_, _) |
            Self::Call(_, _) |
            Self::Noop(_) => (),
        }
        vars
    }

    pub fn eval_static(&self) -> Result<Option<Literal>, Error> {
        match self {
            Expr::Binary(left, op, right) => { 
                let left = left.eval_static()?;
                let right = right.eval_static()?;
                match (&op.t, left, right) {
                    (TokenData::EqualsEquals, Some(Literal::B(l)), Some(Literal::B(r))) => Ok(Some(Literal::B(l == r))),
                    (TokenData::EqualsEquals, Some(Literal::I(l)), Some(Literal::I(r))) => Ok(Some(Literal::B(l == r))),
                    (TokenData::EqualsEquals, Some(Literal::F(l)), Some(Literal::F(r))) => Ok(Some(Literal::B(l == r))),
                    (TokenData::Minus, Some(Literal::I(l)), Some(Literal::I(r))) => Ok(Some(Literal::I(l - r))),
                    (TokenData::Minus, Some(Literal::F(l)), Some(Literal::F(r))) => Ok(Some(Literal::F(l - r))),
                    (TokenData::Plus, Some(Literal::I(l)), Some(Literal::I(r))) => Ok(Some(Literal::I(l + r))),
                    (TokenData::Plus, Some(Literal::F(l)), Some(Literal::F(r))) => Ok(Some(Literal::F(l + r))),
                    (TokenData::Slash, Some(Literal::I(l)), Some(Literal::I(r))) => Ok(Some(Literal::I(l / r))),
                    (TokenData::Slash, Some(Literal::F(l)), Some(Literal::F(r))) => Ok(Some(Literal::F(l / r))),
                    (TokenData::Star, Some(Literal::I(l)), Some(Literal::I(r))) => Ok(Some(Literal::I(l * r))),
                    (TokenData::Star, Some(Literal::F(l)), Some(Literal::F(r))) => Ok(Some(Literal::F(l * r))),
                    (TokenData::Greater, Some(Literal::B(l)), Some(Literal::B(r))) => Ok(Some(Literal::B(l > r))),
                    (TokenData::Greater, Some(Literal::I(l)), Some(Literal::I(r))) => Ok(Some(Literal::B(l > r))),
                    (TokenData::Greater, Some(Literal::F(l)), Some(Literal::F(r))) => Ok(Some(Literal::B(l > r))),
                    (TokenData::Smaller, Some(Literal::B(l)), Some(Literal::B(r))) => Ok(Some(Literal::B(l < r))),
                    (TokenData::Smaller, Some(Literal::I(l)), Some(Literal::I(r))) => Ok(Some(Literal::B(l < r))),
                    (TokenData::Smaller, Some(Literal::F(l)), Some(Literal::F(r))) => Ok(Some(Literal::B(l < r))),
                    (TokenData::GreaterOrEquals, Some(Literal::B(l)), Some(Literal::B(r))) => Ok(Some(Literal::B(l >= r))),
                    (TokenData::GreaterOrEquals, Some(Literal::I(l)), Some(Literal::I(r))) => Ok(Some(Literal::B(l >= r))),
                    (TokenData::GreaterOrEquals, Some(Literal::F(l)), Some(Literal::F(r))) => Ok(Some(Literal::B(l >= r))),
                    (TokenData::SmallerOrEquals, Some(Literal::B(l)), Some(Literal::B(r))) => Ok(Some(Literal::B(l <= r))),
                    (TokenData::SmallerOrEquals, Some(Literal::I(l)), Some(Literal::I(r))) => Ok(Some(Literal::B(l <= r))),
                    (TokenData::SmallerOrEquals, Some(Literal::F(l)), Some(Literal::F(r))) => Ok(Some(Literal::B(l <= r))),
                    (TokenData::NotEquals, Some(Literal::B(l)), Some(Literal::B(r))) => Ok(Some(Literal::B(l != r))),
                    (TokenData::NotEquals, Some(Literal::I(l)), Some(Literal::I(r))) => Ok(Some(Literal::B(l != r))),
                    (TokenData::NotEquals, Some(Literal::F(l)), Some(Literal::F(r))) => Ok(Some(Literal::B(l != r))),
                    (TokenData::Or, Some(Literal::B(l)), Some(Literal::B(r))) => Ok(Some(Literal::B(l | r))),
                    (TokenData::Or, Some(Literal::I(l)), Some(Literal::I(r))) => Ok(Some(Literal::I(l | r))),
                    (TokenData::And, Some(Literal::B(l)), Some(Literal::B(r))) => Ok(Some(Literal::B(l & r))),
                    (TokenData::And, Some(Literal::I(l)), Some(Literal::I(r))) => Ok(Some(Literal::I(l | r))),
                    _ => Err(self.to_err(String::from("Unsupported operation. Can not evaluate statically."))),
                }
            },
            Expr::Grouping(g, _) => g.eval_static(),
            Expr::Literal(l, _) => Ok(Some(l.clone())),
            Expr::Variable(t) => Err(self.to_err(String::from("Expression contains variables. Can't evaluate statically."))),
            Expr::Unary(op, e) => if let TokenData::Not = op.t {
                if let Some(Literal::B(b)) = e.eval_static()? {
                    Ok(Some(Literal::B(!b)))
                } else {
                    Err(self.to_err(String::from("Unable to statically invert a non-boolean literal.")))
                }
            } else {
                Err(self.to_err(String::from("Unexpected unary operator.")))
            },
            Expr::Assignment(_, e) => e.eval_static(),
            Expr::Call(_, _) => Err(self.to_err(String::from("Expression contains Calls. Can't evaluate statically."))),
            Expr::Noop(_) => Ok(None),
        }
    }

    pub fn set_var_ids(&mut self, map: &HashMap<String, usize>) {
        match self {
            Self::Binary(left, _, right) => {left.set_var_ids(map); right.set_var_ids(map); },
            Self::Grouping(sub, _) |
            Self::Unary(_, sub) => sub.set_var_ids(map),
            Self::Assignment(var, sub)=> {var.idx = map.get(var.string.as_str()).cloned(); sub.set_var_ids(map)},
            Self::Variable(var) => var.idx = map.get(var.string.as_str()).cloned(),
            Self::Literal(_, _) |
            Self::Call(_, _) |
            Self::Noop(_) => (),
        }
    }

    pub fn get_writes(&self) -> Option<&str> {
        if let Self::Assignment(target,_) = self {
            Some(target.string.as_str())
        } else {
            None
        }
    }

    pub fn get_call_target(&self) -> Option<&str> {
        match self {
            Self::Call(target, _) => Some(target.string.as_str()),
            Self::Assignment(_,left) => left.get_call_target(),
            _ => None,
        }
    }

    pub fn get_call_arguments(&self) -> Vec<&str> {
        let mut result = Vec::new();    
        match self {
            Self::Call(_,arg_vec) => arg_vec.iter().for_each(|e| result.extend(e.get_reads().iter().map(|v| v.clone()))),
            Self::Assignment(_,left) => result.extend(left.get_call_arguments()),
            _ => (),
        }
        result
    }

    pub fn is_nop(&self) -> bool {
        match self {
            Self::Noop(_) => true,
            _ => false
        }
    }

    pub fn to_err(&self, msg:String) -> Error {
        match self {
            Self::Binary(_, tok, _) |
            Self::Grouping(_, tok) |
            Self::Literal(_, tok) |
            Self::Unary(tok, _) |
            Self::Noop(tok) => Error{line:tok.line, col:tok.col, message:msg},
            Self::Variable(tok) |
            Self::Assignment(tok, _) |
            Self::Call(tok, _) => Error{line:tok.line, col:tok.col, message:msg}
        }
    }

    pub fn line_no(&self) -> usize {
        match self {
            Self::Binary(_, tok, _) |
            Self::Grouping(_, tok) |
            Self::Literal(_, tok) |
            Self::Unary(tok, _) |
            Self::Noop(tok) => tok.line,
            Self::Variable(tok) |
            Self::Assignment(tok, _) |
            Self::Call(tok, _) => tok.line
        }
    }
}

pub enum Stmt {
    Method{
        name:LabelToken, 
        preconditions:Option<Expr>, 
        cost:Option<Expr>, 
        body:Box<Stmt>, 
        
    }, 
    Task{
        name:LabelToken, 
        preconditions:Option<Expr>, 
        cost: Option<Expr>,
        body:Box<Stmt>, 
        effects:Option<Box<Stmt>>, 
    }, 
    Block(Vec<Stmt>),
    Expression(Expr),
    Include(Expr)
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
            Stmt::Task {name, preconditions, cost, body, ..} |
            Stmt::Method {name, preconditions, cost, body,..} => {
                write!(f, "{:>lc$}: {:>depth$}", self.stmt.line_no(), ' ', depth=self.depth, lc=self.max_line_count)?;
                write!(f, "TASK {}", name.string)?;
                if let Some(p) = preconditions {
                    write!(f, "({})", p)?;
                }
                if let Some(c) = cost {
                    write!(f, " cost {}", c)?;
                }
                writeln!(f, ":")?;
                StmtFormatter{depth:new_depth, stmt:body, max_line_count:self.max_line_count}.fmt(f)
            },
            Stmt::Block(blk) => {
                
                // writeln!(f)?;
                blk.iter().try_for_each(|stmt| StmtFormatter{depth:new_depth, stmt, max_line_count:self.max_line_count}.fmt(f))
                // write!(f, "block_end\n")
            },
            Stmt::Expression(e) => writeln!(f, "{:>lc$}: {:>depth$}{}", self.stmt.line_no(), ' ', e, depth=self.depth, lc=self.max_line_count),
            Stmt::Include(e) => writeln!(f, "{:>lc$}: {:>depth$}include {}", self.stmt.line_no(), ' ', e, depth=self.depth, lc=self.max_line_count)
        }?;
        if let Stmt::Task{effects:Some(e),..} = self.stmt {
            writeln!(f, "{:>lc$}: {:>depth$}EFFECTS:", self.stmt.line_no(), ' ', depth=self.depth, lc=self.max_line_count)?;
            StmtFormatter{depth:new_depth, stmt:e, max_line_count:self.max_line_count}.fmt(f)?;
        }
        Ok(())
    }
}

impl std::fmt::Debug for StmtFormatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

pub struct OperatorIterator<'a> {
    pos: usize,
    statements: &'a Vec<Stmt>
}

impl<'a> Iterator for OperatorIterator<'a> {
    type Item = &'a Expr;

    fn next(&mut self) -> Option<Self::Item> {
       let r = match self.statements.get(self.pos) {
            Some(Stmt::Expression(e)) => Some(e),
            _ => None,
       };
       self.pos += 1;
       r
    }
}

pub struct MethodIterator<'a> {
    pos: usize,
    statements: &'a Vec<Stmt>
}

impl<'a> Iterator for MethodIterator<'a> {
    type Item = &'a Stmt;

    fn next(&mut self) -> Option<Self::Item> {
       let r = match self.statements.get(self.pos) {
            Some(stmt) => match stmt { Stmt::Method{..} => Some(stmt), _ => None },
            _ => None,
       };
       self.pos += 1;
       r
    }
}


impl Stmt {
    pub fn to_err(&self, msg:String) -> Error {
        match self {
            Stmt::Method{name,..} |
            Stmt::Task{name,..} => Error{line:name.line, col:name.col, message:msg},
            Stmt::Block(blk) => blk.first().expect("Unable to generate error for empty block.").to_err(msg),
            Stmt::Expression(e) |
            Stmt::Include(e) => e.to_err(msg),
        }
    }

    pub fn line_no(&self) -> usize {
        match self {
            Self::Method{name,..} |
            Self::Task{name,..} => name.line,
            Stmt::Block(blk) => blk.first().expect("Unable to get line for empty block").line_no(),
            Stmt::Expression(e) |
            Stmt::Include(e) => e.line_no()
        }
    }
    pub fn name(&self) -> Result<&str, Error> {
        match self {
            Self::Method{name, ..} |
            Self::Task{name, ..} => Ok(name.string.as_str()),
            _ => Err(self.to_err(String::from("Statement is not a Task or a Method.")))
        }
    }

    pub fn cost(&self) -> Result<Option<&Expr>, Error> {
        match self {
            Self::Method{cost:Some(cost),..} | 
            Self::Task{cost:Some(cost),..} => Ok(Some(cost)),
            Self::Method{cost:None,..} | 
            Self::Task{cost:None,..} => Ok(None),
            _ => Err(self.to_err(String::from("Statement is not a Task or a Method.")))
        }
    }

    pub fn cost_mut(&mut self) -> Result<Option<&mut Expr>, Error> {
        match self {
            Self::Method{cost:Some(cost),..} | 
            Self::Task{cost:Some(cost),..} => Ok(Some(cost)),
            Self::Method{cost:None,..} | 
            Self::Task{cost:None,..} => Ok(None),
            _ => Err(self.to_err(String::from("Statement is not a Task or a Method.")))
        }
    }

    pub fn is_task(&self) -> bool {
        match self {
            Self::Task{..} => true,
            _ => false,
        }
    }

    pub fn is_include(&self) -> Option<&Expr> {
        match self {
            Self::Include(s) => Some(s),
            _ => None,
        }
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

    /// Will iterate over all expressions in this statement. 
    /// If this is a composite task it'll iterate over all of the methods and their statements too
    pub fn expressions<'a>(&'a self) -> Result<OperatorIterator<'a>, Error> {
        match self {
            Self::Task{body,..} |
            Self::Method{body,..} => body.expressions(),
            Self::Block(v) =>  Ok(OperatorIterator { pos: 0, statements: v }),
            //Self::Expression(_) => OperatorIterator { pos: 0, statements: &vec![self.clone()] }
            _ => Err(self.to_err(String::from("Statement doesn't support expressions.")))
        }
    }

    pub fn expressions_mut<'a>(&'a mut self) -> Result<&mut Vec<Stmt>, Error> {
        match self {
            Self::Task{body,..} |
            Self::Method{body,..} => body.expressions_mut(),
            Self::Block(v) =>   Ok(v),
            //Self::Expression(_) => OperatorIterator { pos: 0, statements: &vec![self.clone()] }
            _ => Err(self.to_err(String::from("Statement doesn't support expressions.")))
        }
    }

    pub fn methods<'a>(&'a self) -> Result<MethodIterator<'a>, Error> {
        match self {
            Self::Task{body,..} => body.methods(),
            Self::Block(v) => Ok(MethodIterator { pos: 0, statements: v }),
            // Self::Method{..} => Ok(MethodIterator{pos:0, statements: &vec![self.clone()]}),
            _ => Err(self.to_err(String::from("Expression statement can't have any methods.")))
        }
    }

    pub fn methods_mut<'a>(&'a mut self) -> Result<&mut Vec<Stmt>, Error> {
        match self {
            Self::Task{body,..} => body.methods_mut(),
            Self::Block(v) => Ok(v),
            // Self::Method{..} => Ok(MethodIterator{pos:0, statements: &vec![self.clone()]}),
            _ => Err(self.to_err(String::from("Expression statement can't have any methods.")))
        }
    }

    pub fn preconditions(&self) -> Result<&Option<Expr>, Error> {
        match self {
            Self::Method{preconditions, ..} |
            Self::Task{preconditions, ..} => Ok(preconditions),
            _ => Err(self.to_err(String::from("Statement is not a Task or a Method.")))
        }
    }

    pub fn preconditions_mut(&mut self) -> Result<&mut Option<Expr>, Error> {
        match self {
            Self::Method{preconditions, ..} |
            Self::Task{preconditions, ..} => Ok(preconditions),
            _ => Err(self.to_err(String::from("Statement is not a Task or a Method.")))
        }
    }

    pub fn effects(&self) -> Result<&Option<Box<Stmt>>, Error> {
        match self {
            Self::Task{ref effects,..} => Ok(effects),
            _ => Err(self.to_err(String::from("Only tasks have effects")))
        }
    }

    pub fn effects_mut(&mut self) -> Result<&mut Option<Box<Stmt>>, Error> {
        match self {
            Self::Task{ref mut effects,..} => Ok(effects),
            _ => Err(self.to_err(String::from("Only tasks have effects")))
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
            if let TokenData::Task = self.tokens[self.idx].t {
                return;
            }
            self.idx += 1;
        }
    }
    fn primary(&mut self) -> Result<Expr, Error> {
        // Literal | "(" expression ")"
        if let TokenData::Literal(ref val)  = self.tokens[self.idx].t {
            self.idx += 1;
            Ok(Expr::Literal(val.clone(), self.tokens[self.idx].clone()))
        } else if let TokenData::OpenParenthesis = self.tokens[self.idx].t {
            self.idx += 1;
            let expr = self.expression()?;
            pexpect!(self, TokenData::CloseParenthesis, {
                Ok(Expr::Grouping(Box::new(expr), self.tokens[self.idx].clone()))
            }, "Expected ')' after expression.")
        } else if let TokenData::Label(_) = &self.tokens[self.idx].t {
            self.idx += 1;
            Ok(Expr::Variable(self.tokens[self.idx-1].clone().try_into()?))
        } else if let TokenData::Pass = &self.tokens[self.idx].t {
            self.idx += 1;
            Ok(Expr::Noop(self.tokens[self.idx-1].clone()))
        } else {
            perror!(self, 0, "Expected expression.")
        }

    }
    fn call(&mut self) -> Result<Expr, Error> {
        let expr = self.primary()?;
        ptest!(self, TokenData::OpenParenthesis, {
            let mut args = Vec::<Expr>::new();
            loop { 
                ptest!(self, TokenData::CloseParenthesis, {
                    break if let Expr::Variable(name) = expr {
                        Ok(Expr::Call(name, args))
                    } else {
                        perror!(self, 1, "Expected function name before call.")
                    }
                } else {
                    args.push(self.expression()?);
                    ptest!(self, TokenData::Comma, {} else {});
                })
            }
        } else {
            Ok(expr)
        })
    }
    fn unary(&mut self) -> Result<Expr, Error> {
        ptest!(self, (TokenData::Not | TokenData::Minus), {
            let operator = self.tokens[self.idx-1].clone();
            let right = self.unary()?;
            Ok(Expr::Unary(operator, Box::new(right)))
        } else {
            self.call()
        }) 
    }
    fn factor(&mut self) -> Result<Expr, Error> {
        let mut expr = self.unary()?;
        while let TokenData::Slash | TokenData::Star  = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn term(&mut self) -> Result<Expr, Error> {
        let mut expr = self.factor()?;
        while let TokenData::Minus | TokenData::Plus  = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn comparison(&mut self) -> Result<Expr, Error> {
        let mut expr = self.term()?;
        while let TokenData::Greater | TokenData::GreaterOrEquals | TokenData::Smaller | TokenData::SmallerOrEquals = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn equality(&mut self) -> Result<Expr, Error> {
        let mut expr = self.comparison()?;
        while let TokenData::NotEquals | TokenData::EqualsEquals = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn logic(&mut self) -> Result<Expr, Error> {
        let mut expr = self.equality()?;
        while let TokenData::Or | TokenData::And = self.tokens[self.idx].t {
            let operator = self.tokens[self.idx].clone();
            self.idx += 1;
            let right = self.equality()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn assignment(&mut self) -> Result<Expr, Error> {
        let target = self.logic()?;
        ptest!(self, (TokenData::Equals 
            | TokenData::AddTo  
            | TokenData::SubtractFrom
            | TokenData::DivideBy
            | TokenData::MultiplyBy), {
                if let Expr::Variable(varname) = target {
                    let line = self.tokens[self.idx-1].line;
                    let col = self.tokens[self.idx-1].col;
                    let value_expr = match self.tokens[self.idx-1].t {
                        // TokenData::EQUALS => self.expression()?,
                        TokenData::AddTo => Expr::Binary(Box::new(Expr::Variable(varname.clone())), Token{line, col, len:0, t:TokenData::Plus}, Box::new(self.expression()?)),
                        TokenData::SubtractFrom => Expr::Binary(Box::new(Expr::Variable(varname.clone())), Token{line, col, len:0, t:TokenData::Minus}, Box::new(self.expression()?)),
                        TokenData::MultiplyBy => Expr::Binary(Box::new(Expr::Variable(varname.clone())), Token{line, col, len:0, t:TokenData::Star}, Box::new(self.expression()?)),
                        TokenData::DivideBy => Expr::Binary(Box::new(Expr::Variable(varname.clone())), Token{line, col, len:0, t:TokenData::Slash}, Box::new(self.expression()?)),
                        _ => self.expression()?,
                    };
                    Ok(Expr::Assignment(varname, Box::new(value_expr)))
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
        pexpect_prevtoken!(self, TokenData::Colon, {self.statement(None)}, "Expected ':' after 'effects'.")
    }
    fn task_statement(&mut self) -> Result<Stmt, Error> {
        // let token_id = self.idx-1;
        pexpect!(self, TokenData::Label(_), {
            let name: LabelToken = self.tokens[self.idx-1].clone().try_into()?;
            let mut preconditions = None;
            pmatch!(self, TokenData::OpenParenthesis, {
                preconditions = Some(self.expression()?);
                pexpect!(self, TokenData::CloseParenthesis, {Ok(())}, "Expected ')' after task conditions.")?
            });
            let mut cost = None;
            pmatch!(self, TokenData::Cost, {
                cost = Some(self.expression()?);
            });
            pexpect_prevtoken!(self, TokenData::Colon, {
                let body = Box::new(self.statement(Some(name.string.as_str()))?);
                let mut effects = None;
                pmatch!(self, TokenData::Effects, {effects = Some(Box::new(self.effects_statement()?));});
                Ok(Stmt::Task{name, preconditions, cost, body, effects})
            }, "Expected ':' after task label.")
        }, "Expected label after 'task'.")
        
    }
    fn expression_statement(&mut self) -> Result<Stmt, Error> {
        let expr = self.expression()?;
        pexpect!(self, 
            (TokenData::StatementEnd | TokenData::BlockStart | TokenData::BlockEnd), 
            {Ok(Stmt::Expression(expr))}, 
            "Expected new line after expression.")
    }
    fn block_statement(&mut self, parent_task:Option<&str>) -> Result<Stmt, Error> {
        let mut stmts = Vec::<Stmt>::new();
        loop {
            let mut stmt = self.statement(parent_task)?;
            if let Stmt::Block(ref mut inner) = stmt {
                stmts.append(inner);
            } else {
                stmts.push(stmt);
            }

            if let TokenData::BlockEnd | TokenData::EOF = self.tokens[self.idx].t {
                self.idx += 1;
                return Ok(Stmt::Block(stmts))
            } 
        }
        
    }
    fn method_statement(&mut self, parent_task:Option<&str>) -> Result<Stmt, Error> {
        let parent_task = parent_task.unwrap();
        pexpect!(self, TokenData::Label(_), {
            let mut name: LabelToken = self.tokens[self.idx-1].clone().try_into()?;
            let else_name = format!("{}.Dont{}", parent_task, name.string);
            name.string = format!("{}.{}", parent_task, name.string);
            let mut preconditions: Option<Expr> = None;
            pmatch!(self, TokenData::OpenParenthesis, {
                preconditions = Some(self.expression()?);
                pexpect!(self, TokenData::CloseParenthesis, {Ok(())}, "Expected ')' after method conditions.'")?;
            });
            let mut cost = None;
            pmatch!(self, TokenData::Cost, {
                cost = Some(self.expression()?);
            });
            pexpect_prevtoken!(self, TokenData::Colon, {
                
                
                if preconditions.is_some() {
                    let parent_statemet = Stmt::Method{name, preconditions:preconditions.clone(), cost, body:Box::new(self.statement(None)?)};
                    ptest!(self, TokenData::Else, {
                        let else_token = &self.tokens[self.idx-1];
                        let else_name = LabelToken{line:else_token.line, col:else_token.col, len:else_token.len, string:else_name, idx:None};
                        let mut elsecost = None;
                        pmatch!(self, TokenData::Cost, {
                            elsecost = Some(self.expression()?);
                        });
                        let token_id = self.idx-1;
                        pexpect!(self, TokenData::Colon, {
                            let else_conditions = Expr::Unary(Token{line:self.tokens[token_id].line, col:self.tokens[token_id].col, len:0, t:TokenData::Not}, Box::new(preconditions.unwrap().clone()));
                            let else_statement = Stmt::Method{name:else_name, preconditions:Some(else_conditions), cost:elsecost, body:Box::new(self.statement(None)?)};
                            Ok(Stmt::Block(vec![parent_statemet, else_statement]))
                        }, "Expected ':' after else clause.")
                    } else {
                        Ok(parent_statemet)
                    })
                } else {
                    Ok(Stmt::Method{name, preconditions, cost, body:Box::new(self.statement(None)?)})
                }
            }, "Expected ':' after method declaration.")
        }, "Expected method name.")
    }
    fn include_statement(&mut self) -> Result<Stmt, Error> {
        let expr = self.expression()?;
        pexpect!(self, 
            (TokenData::StatementEnd | TokenData::BlockStart | TokenData::BlockEnd), 
            {Ok(Stmt::Include(expr))}, 
            "Expected new line after expression.")
    }
    fn statement(&mut self, parent:Option<&str>) -> Result<Stmt, Error> {
        // println!("When Parsing a new statement, next token is {}.", self.tokens[self.idx]);
        let r = if let TokenData::Task = self.tokens[self.idx].t {
            self.idx += 1;
            self.task_statement()
        } else if let TokenData::BlockStart = self.tokens[self.idx].t {
            self.idx += 1;
            self.block_statement(parent)
        } else if let TokenData::Method = self.tokens[self.idx].t {
            self.idx += 1;
            self.method_statement(parent)
        } else if let TokenData::Include = self.tokens[self.idx].t {
            self.idx += 1;
            self.include_statement()
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
                    TokenData::BlockStart => {depth += 1; format!("{:?}", item)},
                    TokenData::BlockEnd => {format!("{:?}\n", item)},
                    TokenData::Colon => format!("{:?}\n", item),
                    _ => format!("{:?} ", item),
                };
                let r = if acc.ends_with('\n') || acc.len() == 0 {
                    acc + &format!("{:4}: {:width$}{}", item.line, ' ', item_string, width=depth*4)
                } else if acc.ends_with('{') {
                    acc + &format!(" {}", item_string)
                } else {
                    acc + &format!("{}", item_string)
                };
                if let TokenData::BlockEnd = item.t {
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
        Parser::print_tokens(&parser.tokens);
        while parser.idx + 1 < parser.tokens.len() {
            match parser.statement(None) {
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