use logos::Lexer;
use crate::parser::tokens::Token;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Numeric {
    Typed(NType, NValue),
    UnTyped(NValue),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum NValue {
    Integer(i64),
    Real(f64),
    Binary(i64),
    Octal(i64),
    Hex(i64),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum NType {
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


pub(crate) fn number<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Numeric, String> {
    let x = lex.slice();
    println!("{}", x);
    Ok(Numeric::UnTyped(NValue::Integer(1)))
    //
    // x
    //     .parse::<i64>()
    //     .map(|r| Number::Int(r))
    //     .map_err(|s| s.to_string())
}
//
// pub(crate)  fn real<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Number, String> {
//     lex.slice()
//         .parse::<f64>()
//         .map(|r| Number::Real(r))
//         .map_err(|s| s.to_string())
// }
//
// pub(crate)  fn binary<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Number, String> {
//     isize::from_str_radix(&lex.slice()[2..], 2)
//         .map(Number::Binary)
//         .map_err(|s| s.to_string())
// }
//
// pub(crate) fn hex16<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Number, String> {
//     i64::from_str_radix(lex.slice().trim_start_matches("16#"), 16)
//         .map(|r| Number::Hex(r))
//         .map_err(|s| s.to_string())
// }
//
// pub(crate) fn hex8<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Number, String> {
//     i64::from_str_radix(lex.slice().trim_start_matches("8#"), 8)
//         .map(|r| Number::Hex(r))
//         .map_err(|s| s.to_string())
// }
//
