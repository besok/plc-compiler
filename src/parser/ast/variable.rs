use crate::parser::ast::expression::Expression;
use crate::parser::ast::Id;


/*
variable : symbolic_var | direct_var
symbolic_var : (this .| (id .)+)? (var_access | multi_elem)
var_access : id | id^+
multi_elem : var_access (subscript_list | . var_access)+
*/

#[derive(Debug, Clone, PartialEq)]
pub enum Variable<'a> {
    Direct(&'a str),
    Symbolic(SymbolicVar<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolicVar<'a> {
    pub path: VarPath<'a>,
    pub var: VarValue<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarAccess<'a> {
    Value(Id<'a>),
    Ref(Box<VarAccess<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarValue<'a> {
    Single(VarAccess<'a>),
    Multi(VarAccess<'a>,Vec<MultiVarElem<'a>>)
}
#[derive(Debug, Clone, PartialEq)]
pub enum MultiVarElem<'a> {
    Access(VarAccess<'a>),
    Subscript(Vec<Expression<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarPath<'a> {
    This,
    None,
    Path(Vec<Id<'a>>),
}
