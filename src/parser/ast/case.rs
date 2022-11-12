use crate::parser::ast::numeric::Numeric;
use crate::parser::ast::{Id, SubRange, TypeCast};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum CaseCondition<'a> {
    Subrange(SubRange),
    Num(Numeric),
    Cast(TypeCast<'a>),
    Id(Id<'a>),
}