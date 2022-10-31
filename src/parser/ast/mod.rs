pub(crate) mod time;
pub(crate) mod numeric;

use std::fmt::{Display, Formatter};
use logos::Lexer;
use crate::parser::tokens::Token;

trait Show {
    fn show(&self) -> String;
}
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Id<'a>(&'a str);

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct TypeCast<'a>(Id<'a>, Id<'a>);
pub(crate) fn to_type_cast<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<TypeCast<'a>, String> {
    let x = lex.slice();
    println!("{}", x);
    Ok(TypeCast(Id(""),Id("")))

}


#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Text<'a>(&'a str);

pub(crate) fn to_text<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Text<'a>, String> {
    let x = lex.slice();
    println!("{}", x);
    Ok(Text(""))

}