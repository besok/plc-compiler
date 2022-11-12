use parsit::test::parser_test::{expect, expect_pos, fail_on};
use crate::parser::ast::case::CaseCondition;
use crate::parser::ast::numeric::{NType, Numeric, NValue};
use crate::parser::ast::{Id, SubRange, TypeCast};
use crate::parser::Parser;
mod variable;
mod statement;

pub(crate) fn parser(input: &str) -> Parser {
    Parser::new(input).unwrap()
}

#[test]
fn subrange() {
    expect_pos(parser("1..2").subrange(0), 3);
    expect_pos(parser("2#1111..8#2").subrange(0), 3);
}

#[test]
fn case_cond() {
    expect(parser("1").case_cond(0), CaseCondition::Num(Numeric::UnTyped(NValue::Integer(1))));
    expect(parser("1..2").case_cond(0), CaseCondition::Subrange(SubRange(Numeric::UnTyped(NValue::Integer(1)), Numeric::UnTyped(NValue::Integer(2)))));
    expect(parser("int#2#1..2").case_cond(0), CaseCondition::Subrange(SubRange(Numeric::Typed(NType::INT, NValue::Binary(1)), Numeric::UnTyped(NValue::Integer(2)))));
    expect(parser("a#b").case_cond(0), CaseCondition::Cast(TypeCast(Id("a"), Id("b"))));
    expect(parser("a").case_cond(0), CaseCondition::Id(Id("a")));
}


#[test]
fn subscript_list() {
    expect_pos(parser("[a,b,c]").subscript_list(0), 7);
    expect_pos(parser("[a]").subscript_list(0), 3);
    fail_on(parser("[]").subscript_list(0), 1);
}
