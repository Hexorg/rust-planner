use enumset::{EnumSet, EnumSetType};

#[derive(PartialEq, Debug)]
pub enum Stmt<'a> {
    Domain(Domain<'a>),
    Problem(Problem<'a>)
}

#[derive(PartialEq, Debug)]
pub struct Problem<'a> {
    pub name: &'a str,
    pub domain: &'a str,
    pub requirements: EnumSet<Requirements>,
    pub objects: Vec<TypedList<'a>>,
    pub init: Expr<'a>,
    pub goal: Expr<'a>,
    // pub constraints: &'a str,
    // pub metric: &'a str,
    // pub length: &'a str
}

#[derive(PartialEq, Debug)]
pub struct Domain<'a> {
    pub name: &'a str,
    pub requirements: EnumSet<Requirements>,
    pub types: Vec<TypedList<'a>>,
    // pub constants: TypedList<'a>,
    pub predicates: Vec<Predicate<'a>>,
    // pub functions: &'a str,
    // pub constraints: &'a str,
    pub actions: Vec<Action<'a>>,
}

#[derive(EnumSetType, Debug)]
pub enum Requirements {
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
    Constraints
}

#[derive(PartialEq, Debug)]
pub enum Expr<'a> {
    And(Vec<Expr<'a>>),
    Not(Box<Expr<'a>>),
    Literal{name:&'a str, variables:Vec<&'a str>}
}

#[derive(PartialEq, Debug)]
pub struct Action<'a> {
    pub name: &'a str,
    pub parameters: Vec<TypedList<'a>>,
    pub precondition: Option<Expr<'a>>,
    pub effect: Option<Expr<'a>>
}

#[derive(PartialEq, Debug)]
pub struct Predicate<'a> {
    pub name: &'a str,
    pub variables: Vec<TypedList<'a>>
}

#[derive(PartialEq, Debug)]
pub struct TypedList<'a> {
    pub identifiers: Vec<&'a str>,
    pub name: &'a str,
}
