use std::str::{FromStr, Split};
use logos::Lexer;

use crate::parser::tokens::Token;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct DateValue {
    pub year: i32,
    pub month: u8,
    pub day: u8,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct TimeValue {
    pub hour: u32,
    pub minute: u32,
    pub second: u32,
    pub nano: u32,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct DurationValue {
    pub day: f64,
    pub hour: f64,
    pub min: f64,
    pub sec: f64,
    pub milli: f64,
    pub micro: f64,
    pub nano: u32,
    pub negative: bool,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Date {
    Duration(DurationValue),
    Date(DateValue),
    Time(TimeValue),
    DateTime(DateValue, TimeValue),
}

fn parse<F: FromStr>(input: &str) -> Result<F, String> {
    input.parse::<F>().map_err(|_| format!("impossible to parse {}", input.to_string()))
}

fn trim_prefix(input: &str) -> Result<&str, String> {
    let elems: Vec<&str> = input.split("#").collect();
    match elems[..] {
        [_, val] => Ok(val),
        _ => Err(format!("The wrong numbers of this array"))
    }
}

pub(crate) fn to_duration<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Date, String> {
    const POS_D: usize = 0;
    const POS_H: usize = 1;
    const POS_M: usize = 2;
    const POS_S: usize = 3;
    const POS_MS: usize = 4;
    const POS_US: usize = 5;
    const POS_NS: usize = 6;

    let v: &str = trim_prefix(lex.slice())?;

    let mut chars = v.char_indices();
    let mut char = chars.next();

    let is_negative =
        if let Some((_, '-')) = char {
            char = chars.next();
            true
        } else { false };

    let mut values: [Option<f64>; 7] = [None, None, None, None, None, None, None];

    let mut prev_pos = 0;
    while char.is_some() {
        let number = {
            let start = char.expect("char").0;
            char = chars.find(|(_, ch)| !ch.is_ascii_digit() && !ch.eq(&'.'));
            char.ok_or_else(|| "Invalid TIME Literal: Cannot parse segment.".to_string())
                .and_then(|(index, _)| parse::<f64>(&v[start..index]))?
        };

        let unit = {
            let start = char.map(|(index, _)| index)
                .ok_or_else(|| {
                    "Invalid TIME Literal: Missing unit (d|h|m|s|ms|us|ns)".to_string()
                })?;

            char = chars.find(|(_, ch)| !ch.is_ascii_alphabetic());
            &v[start..char.unwrap_or((v.len(), ' ')).0]
        };

        let position = match unit {
            "d" => Some(POS_D),
            "h" => Some(POS_H),
            "m" => Some(POS_M),
            "s" => Some(POS_S),
            "ms" => Some(POS_MS),
            "us" => Some(POS_US),
            "ns" => Some(POS_NS),
            _ => None,
        };
        if let Some(position) = position {
            if prev_pos > position {
                return Err(
                    "Invalid TIME Literal: segments out of order, use d-h-m-s-ms".to_string(),
                );
            }
            prev_pos = position;

            if values[position].is_some() {
                return Err("Invalid TIME Literal: segments must be unique".to_string());
            }
            values[position] = Some(number); //store the number
        } else {
            return Err(format!("Invalid TIME Literal: illegal unit '{}'", unit));
        }
    }

    Ok(Date::Duration(DurationValue {
        day: values[POS_D].unwrap_or_default(),
        hour: values[POS_H].unwrap_or_default(),
        min: values[POS_M].unwrap_or_default(),
        sec: values[POS_S].unwrap_or_default(),
        milli: values[POS_MS].unwrap_or_default(),
        micro: values[POS_US].unwrap_or_default(),
        nano: values[POS_NS].map(|it| it as u32).unwrap_or(0u32),
        negative: is_negative,
    }))
}

pub(crate) fn to_date_value<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Date, String> {
    let v: &str = lex.slice();
    Ok(Date::Date(parse_date(trim_prefix(v)?)?))
}


pub(crate) fn to_tod_value<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Date, String> {
    let v: &str = lex.slice();
    Ok(Date::Time(parse_tod(trim_prefix(v)?)?))
}

fn parse_tod(input: &str) -> Result<TimeValue, String> {
    let date_elems: Vec<&str> = input.split(":").collect();
    match date_elems[..] {
        [h, m] => {
            Ok(TimeValue {
                hour: parse::<u32>(h)?,
                minute: parse::<u32>(m)?,
                second: 0,
                nano: 0,
            })
        }
        [h, m, s] => {
            let sec = parse::<f64>(s)?;
            let nano = (sec.fract() * &1e+9_f64).round() as u32;
            Ok(TimeValue {
                hour: parse::<u32>(h)?,
                minute: parse::<u32>(m)?,
                second: sec.floor() as u32,
                nano,
            })
        }
        _ => Err(format!("The wrong numbers of this array"))
    }
}

fn parse_date(input: &str) -> Result<DateValue, String> {
    let date_elems: Vec<&str> = input.split("-").collect();
    match date_elems[..] {
        [y, m, d] => {
            Ok(
                DateValue {
                    year: parse::<i32>(y)?,
                    month: parse::<u8>(m)?,
                    day: parse::<u8>(d)?,
                }
            )
        }
        _ => Err(format!("The wrong numbers of this array"))
    }
}

pub(crate) fn to_data_time_value<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Date, String> {
    let v = lex.slice();
    let date_time = trim_prefix(v)?;
    let idx = date_time.rfind("-").expect("the syntax for date and time is wrong");

    let date = parse_date(&date_time[0..idx])?;
    let time = parse_tod(&date_time[idx + 1..])?;

    Ok(Date::DateTime(date, time))
}
