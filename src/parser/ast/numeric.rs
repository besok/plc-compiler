use std::str::FromStr;
use logos::Lexer;
use crate::parser::tokens::Token;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Numeric {
    Typed(NType, NValue),
    UnTyped(NValue),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum NValue {
    Integer(i128),
    Real(f64),
    Binary(isize),
    Octal(isize),
    Hex(isize),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum NType {
    REAL,
    LREAL,
    UINT,
    USINT,
    UDINT,
    ULINT,
    SINT,
    INT,
    DINT,
    LINT,
    BOOL,
    BYTE,
    WORD,
    DWORD,
    LWORD,
    DW,
    B,
    W,
}

impl FromStr for NType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_uppercase().as_str() {
            "REAL" => Ok(NType::REAL),
            "LREAL" => Ok(NType::LREAL),
            "UINT" => Ok(NType::UINT),
            "USINT" => Ok(NType::USINT),
            "UDINT" => Ok(NType::UDINT),
            "ULINT" => Ok(NType::ULINT),
            "SINT" => Ok(NType::SINT),
            "INT" => Ok(NType::INT),
            "DINT" => Ok(NType::DINT),
            "LINT" => Ok(NType::LINT),
            "BOOL" => Ok(NType::BOOL),
            "BYTE" => Ok(NType::BYTE),
            "WORD" => Ok(NType::WORD),
            "DWORD" => Ok(NType::DWORD),
            "LWORD" => Ok(NType::LWORD),
            "DW" => Ok(NType::DW),
            "B" => Ok(NType::B),
            "W" => Ok(NType::W),
            _ => Err("Wrong string".to_string())
        }
    }
}

pub(crate) fn to_number<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Numeric, String> {
    let v:&str = lex.slice();
    let elems: Vec<&str> = v.split("#").collect();
    match elems[..] {
        [int] => Ok(Numeric::UnTyped(parse_int(int)?)),
        ["2", val] => Ok(Numeric::UnTyped(parse_bin(val)?)),
        ["8", val] => Ok(Numeric::UnTyped(parse_oct(val)?)),
        ["16", val] => Ok(Numeric::UnTyped(parse_hex(val)?)),
        [tpe, int] => Ok(Numeric::Typed(tpe.parse()?, parse_int(int)?)),
        [tpe, "2", val] => Ok(Numeric::Typed(tpe.parse()?, parse_bin(val)?)),
        [tpe, "8", val] => Ok(Numeric::Typed(tpe.parse()?, parse_oct(val)?)),
        [tpe, "16", val] => Ok(Numeric::Typed(tpe.parse()?, parse_hex(val)?)),
        _ => Err(format!("The wrong numbers of this array "))
    }
}


pub(crate) fn to_real<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Numeric, String> {
    let v: &str = lex.slice();
    let elems: Vec<&str> = v.split("#").collect();
    match elems[..] {
        [num] => Ok(Numeric::UnTyped(parse_real(num)?)),
        [tpe, num] => Ok(Numeric::Typed(tpe.parse()?, parse_real(num)?)),
        _ => Err(format!("The wrong numbers of this array "))
    }
}

fn parse_real(input: &str) -> Result<NValue, String> {
    input.replace("_","").parse::<f64>().map_err(|err| err.to_string()).map(NValue::Real)
}

fn parse_int(input: &str) -> Result<NValue, String> {
    input.replace("_","").parse::<i128>().map_err(|err| err.to_string()).map(NValue::Integer)
}

fn parse_bin<'a>(input: &str) -> Result<NValue, String> {
    isize::from_str_radix(input.replace("_","").as_str(), 2)
        .map_err(|s| s.to_string())
        .map(NValue::Binary)
}

fn parse_oct<'a>(input: &str) -> Result<NValue, String> {
    isize::from_str_radix(input.replace("_","").as_str(), 8)
        .map_err(|s| s.to_string())
        .map(NValue::Octal)
}

fn parse_hex<'a>(input: &str) -> Result<NValue, String> {
    isize::from_str_radix(input.replace("_","").as_str(), 816)
        .map_err(|s| s.to_string())
        .map(NValue::Hex)
}


