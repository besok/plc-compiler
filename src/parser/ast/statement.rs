/*
table 71-72

stmt : assign_stmt | subprog_ctrl_stmt | selection_stmt | iteration_stmt

assign_stmt : assign | ref_assign | assign_attempt
assign : variable := expression
ref_addr : REF (symbolic_variable | fb_instance_name | class_instance_name) <= covers by symbolic_variable
fb_instance_name : (id .)* var_access              <= covers by symbolic_variable
class_instance_name : (id .)* var_access           <= covers by symbolic_variable
ref_assign : id := id | deref | ref_addr | NULL // ref_addr = ref symbolic_var
assign_attempt : (id | deref) ?= id | deref | ref_addr | NULL

subprog_ctrl_stmt: func_call | invocation | super() | return
invocation : invoc_path '(' (param_assign (, param_assign)*)? ')'
invoc_path : fb_instance_name  | this | this_invoc_path                   <= covers by symbolic_variable
param_assign :  (id := )? Expression | ref_assign | not? id => variable
func_call: (id .)* id '(' (param_assign (, param_assign)*)? ')'

stmt_list: (stmt? ;)*

selection_stmt: if_stmt | case_stmt;
if_stmt: if expression then stmt_list (elsif expression then stmt_list)* (else stmt_list)? end_if

case_stmt: case expression of case_selection+ else stmt_list end_case
case_selection : case_list : stmt_list
case_list: case_list_elem (, case_list_elem)*
case_list_elem: subrange | expression

*/

use crate::parser::ast::expression::Expression;
use crate::parser::ast::{Id, SubRange};
use crate::parser::ast::variable::{SymbolicVar, VarAccess, Variable};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    Assignment(AssignmentSt<'a>),
    Invocation(InvocationSt<'a>),
    Selection(SelectionSt<'a>),
    Iteration,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentSt<'a> {
    JustAssign(Variable<'a>, Expression<'a>),
    RefAssign(Id<'a>, AssignRhs<'a>),
    AssignAttempt(VarAccess<'a>, AssignRhs<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignRhs<'a> {
    Null,
    Var(VarAccess<'a>),
    Addr(SymbolicVar<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum InvocationSt<'a> {
    Invocation(InvocationLine<'a>),
    Super,
    Return,
    FnCall(Vec<Id<'a>>, Vec<Param<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct InvocationLine<'a> {
    pub path: InvocationPath<'a>,
    pub params: Vec<Param<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InvocationPath<'a> {
    This,
    Variable(SymbolicVar<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Param<'a> {
    Assign(Id<'a>, Expression<'a>),
    RefAssign(Id<'a>, AssignRhs<'a>),
    Expression(Expression<'a>),
    OutAssign(Id<'a>, Variable<'a>),
    NotOutAssign(Id<'a>, Variable<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SelectionSt<'a> {
    If(IfSt<'a>),
    Case(CaseSt<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfBranch<'a> {
    pub cond: Expression<'a>,
    pub body: Vec<Statement<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfSt<'a> {
    pub main: IfBranch<'a>,
    pub others: Vec<IfBranch<'a>>,
    pub else_body: Vec<Statement<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CaseSt<'a> {
    pub cond:Expression<'a>,
    pub selections:Vec<CaseSelection<'a>>,
    pub else_body: Vec<Statement<'a>>
}
#[derive(Debug, Clone, PartialEq)]
pub struct CaseSelection<'a> {
    pub elems: Vec<CaseSelectionElem<'a>>,
    pub stmts: Vec<Statement<'a>>
}
#[derive(Debug, Clone, PartialEq)]
pub enum CaseSelectionElem<'a>{
    Subrange(SubRange),
    Expression(Expression<'a>)
}
