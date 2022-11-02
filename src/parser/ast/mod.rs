pub(crate) mod time;
pub(crate) mod numeric;
pub(crate) mod case;

use std::fmt::{Display, Formatter};
use logos::Lexer;
use crate::parser::tokens::Token;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Id<'a>(&'a str);

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct TypeCast<'a>(Id<'a>, Id<'a>);

pub(crate) fn to_type_cast<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<TypeCast<'a>, String> {
    let s: &str = lex.slice();
    let ids: Vec<Id> = s.split("#").map(Id).collect();
    let mut iter = ids.into_iter();
    Ok(TypeCast(iter.next().unwrap(), iter.next().unwrap()))
}


#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Text<'a>(pub &'a str);

pub(crate) fn to_text<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Text<'a>, String> {
    let s: &str = lex.slice();
    let mut chars = s.chars();
    chars.next();
    chars.next_back();
    Ok(Text(chars.as_str()))
}