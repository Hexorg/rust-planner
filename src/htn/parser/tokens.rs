use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal<'a> {
    I(i64),
    F(f64),
    B(bool),
    S(&'a str)
}

impl<'a> Literal<'a> {
    pub fn unwrap_str(self) -> &'a str {
        match self {
            Self::S(s) => s,
            _ => panic!(),
        }
    }
}


impl fmt::Display for Literal<'_> {
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span {
    pub line: usize,
    pub col: usize,
    pub len: usize,
}

impl Span {
    pub fn new(line: usize, col: usize, len:usize) -> Self {
        Self{line, col, len}
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinOpToken {
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
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum KeywordToken {
    Define,
    Domain,
    Problem,
    Requirements,
    Types,
    Objects,
    Predicates,
    Action,
    Init,
    Goal,
    Strips,
    Typing,
    NegativePreconditions,
    DisjunctivePreconditions,
    Equality,
    ExistentialPreconditions,
    UniversalPreconditions,
    QuantifiedPreconditions,
    ConditionalEffects,
    Fluents,
    ADL,
    DurativeActions,
    DerivedPredicates,
    TimedInitialLiterals,
    Preferences,
    Constraints,
    Parameters,
    Precondition,
    Effect
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind<'a> {
    Identifier(&'a str),
    Literal(Literal<'a>),
    BinOp(BinOpToken),
    Keyword(KeywordToken),
    Equals,
    QuestionMark,
    OpenParenthesis,
    CloseParenthesis,
    Colon,
    Comma,
    Dot,
    BlockStart,
    BlockEnd,
    StatementEnd,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Token<'a> {
    pub span: Span,
    pub kind: TokenKind<'a>
}

impl<'a> Token<'a> {
    pub fn unwrap_identifier(self) -> &'a str {
        match self.kind {
            TokenKind::Identifier(s) => s,
            _ => panic!("Expected identifier.")
        }
    }

    pub fn unwrap_literal(self) -> Literal<'a> {
        match self.kind {
            TokenKind::Literal(l) => l,
            _ => panic!("Expected literal.")
        }
    }

}