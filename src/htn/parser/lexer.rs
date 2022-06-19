use super::tokens::{TokenData, Token, Literal};
use super::Error;

enum DepthSeparator {
    TABS,
    SPACES(usize)
}

pub struct Lexer { }

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
                    '.' => r.push(Token{line, col, len:1, t:Dot}),
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
                                            "on" => Some(On),
                                            "as" => Some(As),
                                            "type" => Some(Type),
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
                        } else {
                            if let Some(next_char) = it.peek() {
                                if next_char.is_alphabetic() {
                                    r.push(Token{line, col, len:1, t:Label(String::new())})
                                }
                            }
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
