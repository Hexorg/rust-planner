use std::str::FromStr;

use super::parser::{ParserError, Parser};

pub struct Domain {

}

impl FromStr for Domain {
    type Err = ParserError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Parser::parse(s)
    }
}