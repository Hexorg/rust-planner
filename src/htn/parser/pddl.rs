pub mod lexer;
pub mod ast;

use ast::Stmt;
use enumset::EnumSet;
use lexer::Lexer;




use self::ast::TypedList;

use super::{Error, Position};
use super::tokens::{Token, TokenKind, KeywordToken, BinOpToken};

/// Parses PDDL 3.0 syntax
/// Based on https://github.com/jan-dolejsi/pddl-reference/blob/master/_citedpapers/pddl3bnf.pdf
pub struct Parser<'a> {
    lexer: std::iter::Peekable<lexer::Lexer<'a>>,
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Stmt<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if let None = self.lexer.peek() {
            None
        } else {
            let r = self.root();
            if r.is_err() {
                self.error_recover();
            }
            Some(r)
        }
    }
}

const EXPECTED_IDENTIFIER: &str = "Expected identifier.";
const EXPECTED_COLON:&str = "Expected ':'.";
const EXPECTED_OPEN_PARENTHESIS: &str = "Expected '('.";
const EXPECTED_CLOSE_PARENTHESIS: &str = "Expected matched ')'.";

#[macro_export]
macro_rules! expect {
    ($input:expr, {$($p:pat => $b:expr$(,)?)+}, $err:expr) => {
        match $input {
            $($p => $b,)+
            Some(Ok(Token{span,..})) => Err(Error { pos: Position::Span(span), message:String::from($err) }),
            Some(Err(e)) => Err(e),
            None => Err(Error { pos: Position::EOF, message:String::from($err) })
        }   
    };
}

impl<'a> Parser<'a> {

    fn error_recover(&mut self) {

    }

    fn problem(&mut self) -> Result<Stmt<'a>, Error> {
        use TokenKind::{Keyword, Colon, Identifier, OpenParenthesis, CloseParenthesis};
        use KeywordToken::*;
        let name = expect!(self.lexer.next(), {Some(Ok(Token{kind:Identifier(s),..})) => Ok(s)}, "Expected domain name.")?;
        expect!(self.lexer.next(), {Some(Ok(Token{kind:CloseParenthesis,..})) => Ok(())}, EXPECTED_CLOSE_PARENTHESIS)?;
        let mut domain = None;
        let mut requirements = EnumSet::empty();
        let mut objects = Vec::new();
        let mut init = None;
        let mut goal = None;
        while self.lexer.next_if(|r| matches!(r, Ok(Token{kind:OpenParenthesis,..}))).is_some() {
            expect!(self.lexer.next(), {Some(Ok(Token{kind:Colon,..})) => Ok(())}, EXPECTED_COLON)?;
            expect!(self.lexer.next(), {
                Some(Ok(Token{kind:Keyword(Domain),..})) => Ok(domain = Some(expect!(self.lexer.next(), {Some(Ok(Token{kind:Identifier(s),..}))=>Ok(s)}, EXPECTED_IDENTIFIER)?)),
                Some(Ok(Token{kind:Keyword(Requirements),..})) => Ok(requirements = self.requirements()?),
                Some(Ok(Token{kind:Keyword(Objects),..})) => Ok(objects = self.types()?),
                Some(Ok(Token{kind:Keyword(Init),..})) => Ok(init = Some(self.and()?)), // force it to use a vector of Expressions
                Some(Ok(Token{kind:Keyword(Goal),..})) => Ok(goal = Some(self.expr()?)),
            }, "Expected :domain, :requirements, :objects.")?;
            expect!(self.lexer.next(), {Some(Ok(Token{kind:CloseParenthesis,..})) => Ok(())}, EXPECTED_CLOSE_PARENTHESIS)?;
        }
        let domain = domain.unwrap();
        let init = init.unwrap();
        let goal = goal.unwrap();
        Ok(Stmt::Problem(ast::Problem{name, domain, requirements, objects, init, goal}))
    }

    fn literal(&mut self, name:&'a str) -> Result<ast::Expr<'a>, Error> {
        use TokenKind::{Identifier, QuestionMark};
        let mut variables = Vec::new();
        const EXPECTED_VARIABLE: &str = "Expected variable.";
        match self.lexer.peek() {
            Some(Ok(Token{kind:QuestionMark,..})) => {
                while self.lexer.next_if(|t| matches!(t, Ok(Token{kind:QuestionMark,..}))).is_some() {
                    variables.push(expect!(self.lexer.next(), {
                        Some(Ok(Token{kind:Identifier(s),..})) => Ok(s)
                    }, EXPECTED_IDENTIFIER)?)
                }
                Ok(ast::Expr::Literal { name, variables })
            },
            Some(Ok(Token{kind:Identifier(_),..})) => {
                while let Some(Ok(Token{kind:Identifier(s),..})) = self.lexer.next_if(|t| matches!(t, Ok(Token{kind:Identifier(_),..}))) {
                    variables.push(s);
                }
                Ok(ast::Expr::Literal { name, variables })
            },
            Some(Ok(Token{span,kind})) => Err(Error { pos: Position::Span(*span), message:format!("{} got {:?}", EXPECTED_VARIABLE, kind) }),
            Some(Err(e)) => Err(e.clone()),
            None => Err(Error { pos: Position::EOF, message:String::from(EXPECTED_VARIABLE) })
        }
    }

    fn not(&mut self) -> Result<ast::Expr<'a>, Error> {
        Ok(ast::Expr::Not(Box::new(self.expr()?)))
    }

    fn and(&mut self) -> Result<ast::Expr<'a>, Error> {
        use TokenKind::{OpenParenthesis};
        let mut group = Vec::new();
        while matches!(self.lexer.peek(), Some(Ok(Token{kind:OpenParenthesis,..}))) {
            group.push(self.expr()?)
        }
        Ok(ast::Expr::And(group))
    }

    fn expr(&mut self) -> Result<ast::Expr<'a>, Error> {
        use TokenKind::{BinOp, Identifier, OpenParenthesis, CloseParenthesis};
        use BinOpToken::{And, Not};
        expect!(self.lexer.next(), {Some(Ok(Token{kind:OpenParenthesis,..})) => Ok(())}, EXPECTED_OPEN_PARENTHESIS)?;
        let result = expect!(self.lexer.next(), {
            Some(Ok(Token{kind:BinOp(And),..})) => self.and(),
            Some(Ok(Token{kind:BinOp(Not),..})) => self.not(),
            Some(Ok(Token{kind:Identifier(s),..})) => self.literal(s),
        }, "Expected expression.")?;
        expect!(self.lexer.next(), {Some(Ok(Token{kind:CloseParenthesis,..})) => Ok(())}, EXPECTED_CLOSE_PARENTHESIS)?;
        Ok(result)
    }

    fn action(&mut self) -> Result<ast::Action<'a>, Error> {
        use TokenKind::{Keyword, Identifier, QuestionMark, Colon, OpenParenthesis, CloseParenthesis};
        use KeywordToken::{Parameters, Precondition, Effect};
        let name = expect!(self.lexer.next(), {Some(Ok(Token{kind:Identifier(s),..})) => Ok(s)}, EXPECTED_IDENTIFIER)?;
        let mut parameters = Vec::new();
        expect!(self.lexer.next(), {Some(Ok(Token{kind:Colon,..}))=>Ok(())}, EXPECTED_COLON)?;
        expect!(self.lexer.next(), {Some(Ok(Token{kind:Keyword(Parameters),..})) => Ok(())}, "Expected 'parameters'.")?;
        expect!(self.lexer.next(), {Some(Ok(Token{kind:OpenParenthesis,..})) => Ok(())}, EXPECTED_OPEN_PARENTHESIS)?;
        while let Some(Ok(Token{kind:QuestionMark,..})) = self.lexer.peek() {
            parameters.push(self.typed_list_variable()?);
        }
        expect!(self.lexer.next(), {Some(Ok(Token{kind:CloseParenthesis,..})) => Ok(())}, EXPECTED_CLOSE_PARENTHESIS)?;
        let mut precondition = None;
        let mut effect = None;
        self.lexer.next_if(|t| matches!(t, Ok(Token{kind:Colon,..})));
        if self.lexer.next_if(|t| matches!(t, Ok(Token{kind:Keyword(Precondition),..}))).is_some() {
            precondition = Some(self.expr()?);
        }
        self.lexer.next_if(|t| matches!(t, Ok(Token{kind:Colon,..})));
        if self.lexer.next_if(|t| matches!(t, Ok(Token{kind:Keyword(Effect),..}))).is_some() {
            effect = Some(self.expr()?);
        }
        Ok(ast::Action{name, parameters, precondition, effect})
    }

    fn predicates(&mut self) -> Result<Vec<ast::Predicate<'a>>, Error> {
        use TokenKind::{OpenParenthesis, Identifier, QuestionMark, CloseParenthesis};
        let mut predicates = Vec::new();
        while self.lexer.next_if(|t| matches!(t, Ok(Token{kind:OpenParenthesis,..}))).is_some() {
            let name = expect!(self.lexer.next(), {Some(Ok(Token{kind:Identifier(s),..})) => Ok(s)}, EXPECTED_IDENTIFIER)?;
            let mut variables = Vec::new();
            while let Some(Ok(Token{kind:QuestionMark,..})) = self.lexer.peek() {
                variables.push(self.typed_list_variable()?);
            }
            predicates.push(ast::Predicate{name, variables});
            expect!(self.lexer.next(), {Some(Ok(Token{kind:CloseParenthesis,..}))=>Ok(())}, EXPECTED_CLOSE_PARENTHESIS)?;
        }
        Ok(predicates)
    }

    fn typed_list_variable(&mut self) -> Result<TypedList<'a>, Error> {
        use TokenKind::{BinOp, QuestionMark, Identifier};
        use BinOpToken::Minus;
        let mut identifiers = Vec::new();
        while self.lexer.next_if(|t| matches!(t, Ok(Token{kind:QuestionMark,..}))).is_some() {
            identifiers.push(expect!(self.lexer.next(), {Some(Ok(Token{kind:Identifier(s),..})) => Ok(s)}, EXPECTED_IDENTIFIER)?);
        }
        expect!(self.lexer.next(), {Some(Ok(Token{kind:BinOp(Minus),..})) => Ok(())}, "Expected '-' at the end of typed list.")?;
        let kind = expect!(self.lexer.next(), {Some(Ok(Token{kind:Identifier(s),..})) => Ok(s)}, EXPECTED_IDENTIFIER)?;
        Ok(TypedList{identifiers, kind})
    }

    fn typed_list_name(&mut self) -> Result<TypedList<'a>, Error> {
        use TokenKind::{BinOp, Identifier};
        use BinOpToken::Minus;
        let mut identifiers = Vec::new();
        while let Some(Ok(Token{kind:Identifier(s),..})) = self.lexer.next_if(|t| matches!(t, Ok(Token{kind:Identifier(_),..}))) {
            identifiers.push(s);
        }
        expect!(self.lexer.next(), {Some(Ok(Token{kind:BinOp(Minus),..})) => Ok(())}, "Expected '-' at the end of typed list.")?;
        let kind = expect!(self.lexer.next(), {Some(Ok(Token{kind:Identifier(s),..})) => Ok(s)}, EXPECTED_IDENTIFIER)?;
        Ok(TypedList{identifiers, kind})
    }

    fn types(&mut self) -> Result<Vec<TypedList<'a>>, Error> {
        use TokenKind::*;
        let mut types = Vec::new();
        while let Some(Ok(Token{kind:Identifier(_),..})) = self.lexer.peek() {
            types.push(self.typed_list_name()?);
        }
        Ok(types)
    }

    fn requirements(&mut self) -> Result<EnumSet<ast::Requirements>, Error> {
        use TokenKind::{Keyword, Colon};
        use KeywordToken::*;
        let mut r = EnumSet::empty();
        while self.lexer.next_if(|t| matches!(t, Ok(Token{kind:Colon,..}))).is_some() {
            expect!(self.lexer.next(), {
                Some(Ok(Token{kind:Keyword(Strips),..})) => Ok(r.insert(ast::Requirements::Strips)),
                Some(Ok(Token{kind:Keyword(Typing),..})) => Ok(r.insert(ast::Requirements::Typing)),
                Some(Ok(Token{kind:Keyword(ActionCosts),..})) => Ok(r.insert(ast::Requirements::ActionCosts)),
                Some(Ok(Token{kind:Keyword(NegativePreconditions),..})) => Ok(r.insert(ast::Requirements::NegativePreconditions)),
                Some(Ok(Token{kind:Keyword(DisjunctivePreconditions),..})) => Ok(r.insert(ast::Requirements::DisjunctivePreconditions)),
                Some(Ok(Token{kind:Keyword(Equality),..})) => Ok(r.insert(ast::Requirements::Equality)),
                Some(Ok(Token{kind:Keyword(ExistentialPreconditions),..})) => Ok(r.insert(ast::Requirements::ExistentialPreconditions)),
                Some(Ok(Token{kind:Keyword(UniversalPreconditions),..})) => Ok(r.insert(ast::Requirements::UniversalPreconditions)),
                Some(Ok(Token{kind:Keyword(QuantifiedPreconditions),..})) => Ok(r.insert(ast::Requirements::QuantifiedPreconditions)),
                Some(Ok(Token{kind:Keyword(ConditionalEffects),..})) => Ok(r.insert(ast::Requirements::ConditionalEffects)),
                Some(Ok(Token{kind:Keyword(Fluents),..})) => Ok(r.insert(ast::Requirements::Fluents)),
                Some(Ok(Token{kind:Keyword(ADL),..})) => Ok(r.insert(ast::Requirements::ADL)),
                Some(Ok(Token{kind:Keyword(DurativeActions),..})) => Ok(r.insert(ast::Requirements::DurativeActions)),
                Some(Ok(Token{kind:Keyword(DerivedPredicates),..})) => Ok(r.insert(ast::Requirements::DerivedPredicates)),
                Some(Ok(Token{kind:Keyword(TimedInitialLiterals),..})) => Ok(r.insert(ast::Requirements::TimedInitialLiterals)),
                Some(Ok(Token{kind:Keyword(Preferences),..})) => Ok(r.insert(ast::Requirements::Preferences)),
                Some(Ok(Token{kind:Keyword(Constraints),..})) => Ok(r.insert(ast::Requirements::Constraints)),
            }, "Expected requirements.")?;
        }
        Ok(r)
    }

    fn domain(&mut self) -> Result<Stmt<'a>, Error> {
        use TokenKind::{Keyword, Colon, Identifier, OpenParenthesis, CloseParenthesis};
        use KeywordToken::*;
        let name = expect!(self.lexer.next(), {Some(Ok(Token{kind:Identifier(s),..})) => Ok(s)}, "Expected domain name.")?;
        expect!(self.lexer.next(), {Some(Ok(Token{kind:CloseParenthesis,..})) => Ok(())}, EXPECTED_CLOSE_PARENTHESIS)?;
        let mut requirements = EnumSet::empty();
        let mut types = Vec::new();
        let mut predicates = Vec::new();
        let mut actions = Vec::new();
        while self.lexer.next_if(|r| matches!(r, Ok(Token{kind:OpenParenthesis,..}))).is_some() {
            expect!(self.lexer.next(), {Some(Ok(Token{kind:Colon,..})) => Ok(())}, EXPECTED_COLON)?;
            expect!(self.lexer.next(), {
                Some(Ok(Token{kind:Keyword(Requirements),..})) => Ok(requirements = self.requirements()?),
                Some(Ok(Token{kind:Keyword(Types),..})) => Ok(types = self.types()?),
                Some(Ok(Token{kind:Keyword(Predicates),..})) => Ok(predicates = self.predicates()?),
                Some(Ok(Token{kind:Keyword(Action),..})) => Ok(actions.push(self.action()?)),
            }, "Expected :requirements, :types, :predicates, or :action.")?;
            expect!(self.lexer.next(), {Some(Ok(Token{kind:CloseParenthesis,..})) => Ok(())}, EXPECTED_CLOSE_PARENTHESIS)?;
        }
        Ok(Stmt::Domain(ast::Domain{name, requirements, types, predicates, actions}))
    }
    
    fn root(&mut self)-> Result<Stmt<'a>, Error> {
        use KeywordToken::*;
        use TokenKind::*;

        expect!(self.lexer.next(), {Some(Ok(Token{kind:OpenParenthesis,..})) => Ok(())}, EXPECTED_OPEN_PARENTHESIS)?;
        expect!(self.lexer.next(), {Some(Ok(Token{kind:Keyword(Define),..})) => Ok(())}, "Expected 'define'.")?;
        expect!(self.lexer.next(), {Some(Ok(Token{kind:OpenParenthesis,..})) => Ok(())}, EXPECTED_OPEN_PARENTHESIS)?;
        let body = expect!(self.lexer.next(), {
            Some(Ok(Token{kind:Keyword(Domain),..})) => self.domain(),
            Some(Ok(Token{kind:Keyword(Problem),..})) => self.problem()
        }, "Expected 'domain' or 'problem'.")?;
        expect!(self.lexer.next(), {Some(Ok(Token{kind:CloseParenthesis,..})) => Ok(())}, EXPECTED_CLOSE_PARENTHESIS)?;
        Ok(body)
    }

    pub fn new(code:&'a str) -> Self {
        let lexer = Lexer::<'a>::new(code).peekable();
        Self{lexer}
    }
}

#[cfg(test)]
mod tests {
    use enumset::{EnumSet, enum_set};

    // use crate::htn::parser::{Parser, Error, statement::{Stmt, StmtKind, Block}, expression::{Expr, ExprKind}, tokens::{Span, Literal::*}};
    use super::Parser;
    use super::ast::{Stmt, Domain, Problem, Requirements, TypedList, Predicate, Action, Expr};
    #[test]
    fn test_domain() {
        let code = "(define (domain test) (:requirements :strips :typing) (:types hand - object water - beverage) (:predicates (warm ?o - object)) (:action test :parameters (?h - hand ?b - beverage) :precondition (cold ?h) :effect (warm ?b)))";
        let mut parser = Parser::new(code);
        assert_eq!(parser.next(), Some(Ok(Stmt::Domain(Domain{
            name:"test", 
            requirements:enum_set!(Requirements::Strips | Requirements::Typing), 
            types:vec![TypedList{identifiers:vec!["hand"], kind:"object"},
                       TypedList{identifiers:vec!["water"], kind:"beverage"}], 
            predicates:vec![Predicate{name:"warm", variables:vec![TypedList{identifiers:vec!["o"], kind:"object"}]}], 
            actions:vec![Action{
                name:"test", 
                parameters:vec![TypedList{identifiers:vec!["h"], kind:"hand"}, TypedList{identifiers:vec!["b"], kind:"beverage"}],
                precondition:Some(Expr::Literal { name: "cold", variables: vec!["h"] }),
                effect:Some(Expr::Literal { name: "warm", variables: vec!["b"] })
            }]
        }))));
        assert_eq!(parser.next(), None);
    }

    #[test]
    fn test_problem() {
        let code = "(define (problem test) (:domain barman) (:objects shaker1 - shaker) (:init (ontable shaker1)) (:goal (and (contains shot1 cocktail1))))";
        let mut parser = Parser::new(code);
        assert_eq!(parser.next(), Some(Ok(Stmt::Problem(Problem{
            name:"test",
            domain:"barman",
            requirements: EnumSet::empty(),
            objects:vec![TypedList{identifiers:vec!["shaker1"], kind:"shaker"}],
            init: Expr::And(vec![Expr::Literal { name: "ontable", variables: vec!["shaker1"] }]),
            goal: Expr::And(vec![Expr::Literal { name: "contains", variables: vec!["shot1", "cocktail1"] }])
        }))))
    }
    
}