use parsit::parser::ParseIt;
use parsit::step::Step;
use crate::parser::tokens::Token;

pub(crate) mod tokens;
pub(crate) mod ast;

struct Parser<'a> {
    delegate: ParseIt<'a, Token<'a>>,
}

impl<'a> Parser<'a> {

   pub fn case_cond(&self, pos: usize) -> Step<'a, >{}


}

