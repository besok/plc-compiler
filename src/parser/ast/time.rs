use logos::Lexer;

use crate::parser::tokens::Token;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct DateValue {
    year: i32,
    month: u8,
    day: u8,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct TimeValue {
    hour: u8,
    minute: u8,
    second: u8,
    nano: u32,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct DurationValue {
    hour: u8,
    minute: u8,
    second: u8,
    nano: u32,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Date {
    Duration(DurationValue),
    Date(DateValue),
    Time(TimeValue),
    DateTime(DateValue, TimeValue),
    X
}

pub(crate) fn to_duration<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Date, String> {
    let x = lex.slice();
    println!("{}", x);
    Ok(Date::X)
}
pub(crate) fn to_date_value<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Date, String> {
    let x = lex.slice();
    println!("{}", x);
    Ok(Date::X)
}
pub(crate) fn to_tod_value<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Date, String> {
    let x = lex.slice();
    println!("{}", x);
    Ok(Date::X)
}
pub(crate) fn to_data_time_value<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Date, String> {
    let x = lex.slice();
    println!("{}", x);
    Ok(Date::X)
}
