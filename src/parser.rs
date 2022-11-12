use std::str::FromStr;
use parsit::error::ParseError;
use parsit::parser::ParseIt;
use parsit::step::Step;
use parsit::parser::EmptyToken;
use parsit::{seq, token, wrap};
use crate::parser::ast::case::CaseCondition;
use crate::parser::ast::numeric::Numeric;
use crate::parser::ast::{Id, SubRange, TypeCast};
use crate::parser::ast::expression::Expression;
use crate::parser::ast::statement::{AssignmentSt, AssignRhs, InvocationLine, InvocationPath, Param, InvocationSt, IfSt, Statement, IfBranch};
use crate::parser::ast::variable::{MultiVarElem, SymbolicVar, VarAccess, Variable, VarPath, VarValue};
use crate::parser::tokens::Token;

pub(crate) mod tokens;
pub(crate) mod ast;

#[cfg(test)]
pub(crate) mod tests;


pub struct Parser<'a> {
    delegate: ParseIt<'a, Token<'a>>,
}


impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Result<Self, ParseError<'a>> {
        Ok(Parser {
            delegate: ParseIt::new(src)?,
        })
    }
    fn token(&self, pos: usize) -> Result<(&Token<'a>, usize), ParseError<'a>> {
        self.delegate.token(pos)
    }
}


impl<'a> Parser<'a> {
    pub fn end_if(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::EndIf)
    }
    pub fn semi(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::Semi)
    }
    pub fn then(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::Then)
    }
    pub fn else_(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::Else)
    }
    pub fn els_if(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::ElseIf)
    }
    pub fn if_(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::If)
    }
    pub fn return_(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::Return)
    }
    pub fn super_(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::Super)
    }
    pub fn r_arrow(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::OutAssign)
    }
    pub fn not(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::Not)
    }
    pub fn this(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::This)
    }
    pub fn comma(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::Comma)
    }
    pub fn l_p(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::LParen)
    }
    pub fn r_p(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::RParen)
    }
    pub fn dot(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::Dot)
    }
    pub fn caret(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::Caret)
    }
    pub fn lbr(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::LBrack)
    }
    pub fn rbr(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::RBrack)
    }
    pub fn num(&self, pos: usize) -> Step<'a, Numeric> {
        token!(self.token(pos) => Token::Numeric(n) => *n)
    }
    pub fn id(&self, pos: usize) -> Step<'a, Id<'a>> {
        token!(self.token(pos) => Token::Id(n) => Id(n))
    }
    pub fn cast(&self, pos: usize) -> Step<'a, TypeCast<'a>> {
        token!(self.token(pos) => Token::TypeCastPrefix(n) => *n)
    }
    pub fn direct_variable(&self, pos: usize) -> Step<'a, &'a str> {
        token!(self.token(pos) => Token::DirectVariable(n) => *n)
    }
    pub fn assign(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::Assign)
    }
    pub fn assign_attempt(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::OptAssign)
    }
    pub fn ref_(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::Ref)
    }
    pub fn null(&self, pos: usize) -> Step<'a, EmptyToken> {
        token!(self.token(pos) => Token::Null)
    }
}


impl<'a> Parser<'a> {
    pub fn expression(&self, pos: usize) -> Step<'a, Expression<'a>> {
        token!(self.token(pos) => Token::Nil => Expression::X)
    }
    pub fn subrange(&self, pos: usize) -> Step<'a, SubRange> {
        self.num(pos)
            .then_skip(|p| token!(self.token(p) => Token::EllipsisIn))
            .then_zip(|p| self.num(p))
            .map(|(l, r)| SubRange(l, r))
    }

    pub fn case_cond(&self, pos: usize) -> Step<'a, CaseCondition> {
        self.subrange(pos).map(CaseCondition::Subrange).or_from(pos)
            .or(|p| self.cast(p).map(CaseCondition::Cast))
            .or(|p| self.num(p).map(CaseCondition::Num))
            .or(|p| self.id(p).map(CaseCondition::Id))
            .into()
    }
    pub fn subscript_list(&self, pos: usize) -> Step<'a, Vec<Expression<'a>>> {
        let lb = |p| self.lbr(p);
        let rb = |p| self.rbr(p);
        let comma = |p| self.comma(p);
        let expr = |p| self.expression(p);

        let exprs = |p| seq!(p => expr, comma);
        wrap!(pos => lb; exprs ; rb)
    }

    pub fn var_access(&self, pos: usize) -> Step<'a, VarAccess<'a>> {
        let to_refs_fn = |(id, refs): (Id<'a>, Vec<EmptyToken>)| {
            refs.into_iter()
                .fold(VarAccess::Value(id),
                      |acc, _| VarAccess::Ref(Box::new(acc)),
                )
        };

        self.id(pos)
            .then_multi_zip(|p| self.caret(p))
            .map(to_refs_fn)
    }


    pub fn var_value(&self, pos: usize) -> Step<'a, VarValue<'a>> {
        let multi_elems = |p| {
            self.subscript_list(p).map(MultiVarElem::Subscript)
                .or_from(p)
                .or(|p|
                    self
                        .dot(p)
                        .then(|p| self.var_access(p).map(MultiVarElem::Access)))
                .into()
        };
        let map_res = |(va, multi): (VarAccess<'a>, Vec<MultiVarElem<'a>>)| {
            if multi.is_empty() { VarValue::Single(va) } else {
                VarValue::Multi(va, multi)
            }
        };

        self.var_access(pos).then_multi_zip(multi_elems).map(map_res)
    }
    pub fn var_path(&self, pos: usize) -> Step<'a, VarPath<'a>> {
        self.this(pos).then(|p| self.dot(p)).map(|_| VarPath::This)
            .or_from(pos)
            .or(|p| self.delegate.one_or_more(p, |p| { self.id(p).then_skip(|p| self.dot(p)) }).map(VarPath::Path))
            .into()
    }
    pub fn symbolic_var(&self, pos: usize) -> Step<'a, SymbolicVar<'a>> {
        self.var_path(pos).or_val(VarPath::None)
            .then_zip(|p| self.var_value(p))
            .map(|(path, var)| SymbolicVar { path, var })
    }
    pub fn variable(&self, pos: usize) -> Step<'a, Variable<'a>> {
        let symbolic = |p| self.symbolic_var(p).map(Variable::Symbolic);
        let direct = self.direct_variable(pos).map(Variable::Direct);

        direct.or(symbolic)
    }

    pub fn assign_rhs(&self, pos: usize) -> Step<'a, AssignRhs<'a>> {
        let ref_addr = |p: usize| {
            self.ref_(p).then(|p| self.symbolic_var(p))
        };
        self.null(pos).map(|_| AssignRhs::Null).or_from(pos)
            .or(|p| self.var_access(p).map(AssignRhs::Var))
            .or(|p| ref_addr(p).map(AssignRhs::Addr))
            .into()
    }
    pub fn ref_assign(&self, pos: usize) -> Step<'a, (Id<'a>, AssignRhs<'a>)> {
        self.id(pos)
            .then_skip(|p| self.assign(p))
            .then_zip(|p| self.assign_rhs(p))
    }

    pub fn assignment(&self, pos: usize) -> Step<'a, AssignmentSt<'a>> {
        let assign = |p: usize| {
            self.variable(p)
                .then_skip(|p| self.assign(p))
                .then_zip(|p| self.expression(p))
                .map(|(v, e)| AssignmentSt::JustAssign(v, e))
        };

        let ref_assign = |p: usize| {
            self.ref_assign(p)
                .map(|(id, rhs)| AssignmentSt::RefAssign(id, rhs))
        };

        let assign_attempt = |p: usize| {
            self.var_access(p)
                .then_skip(|p| self.assign_attempt(p))
                .then_zip(|p| self.assign_rhs(p))
                .map(|(id, rhs)| AssignmentSt::AssignAttempt(id, rhs))
        };

        assign(pos)
            .or_from(pos)
            .or(ref_assign)
            .or(assign_attempt)
            .into()
    }
    pub fn param_assign(&self, pos: usize) -> Step<'a, Param<'a>> {
        let expr = |p| self.expression(p).map(Param::Expression);
        let assign = |p| {
            self.id(p)
                .then_skip(|p| self.assign(p))
                .then_zip(|p| self.expression(p))
                .map(|(id, e)| Param::Assign(id, e))
        };
        let ref_assign = |p|
            self.ref_assign(p)
                .map(|(k, v)| Param::RefAssign(k, v));

        let out_assign = |p| {
            self.not(p)
                .or_none()
                .then_zip(|p| self.id(p))
                .then_skip(|p| self.r_arrow(p))
                .then_zip(|p| self.variable(p))
                .map(|((not, id), v)| {
                    if not.is_some() {
                        Param::NotOutAssign(id, v)
                    } else {
                        Param::OutAssign(id, v)
                    }
                })
        };
        out_assign(pos).or_from(pos)
            .or(ref_assign)
            .or(assign)
            .or(expr)
            .into()
    }
    pub fn param_assign_list(&self, pos: usize) -> Step<'a, Vec<Param<'a>>> {
        let param = |p| self.param_assign(p);
        let comma = |p| self.comma(p);
        let l = |p| self.l_p(p);
        let r = |p| self.r_p(p);

        let seq = |p| seq!(p => param, comma);
        let empty = vec![];
        wrap!(pos => l;seq or empty;r)
    }
    pub fn fn_call(&self, pos: usize) -> Step<'a, InvocationSt<'a>> {
        let dot = |p| self.dot(p);
        let id = |p| self.id(p);

        seq!(pos => id,dot)
            .then_zip(|p| self.param_assign_list(p))
            .map(|(ids, params)|
                InvocationSt::FnCall(ids, params))
    }

    pub fn invocation(&self, pos: usize) -> Step<'a, InvocationLine<'a>> {
        let path: Step<InvocationPath> = self.symbolic_var(pos).map(InvocationPath::Variable)
            .or_from(pos)
            .or(|p| self.this(p).map(|_| InvocationPath::This))
            .into();

        path
            .then_zip(|p| self.param_assign_list(p))
            .map(|(path, params)| InvocationLine { path, params })
    }
    pub fn invocation_st(&self, pos: usize) -> Step<'a, InvocationSt<'a>> {
        let super_with = |p|
            self.super_(p)
                .then(|p| self.l_p(p))
                .then(|p| self.r_p(p))
                .map(|_| InvocationSt::Super);

        self.invocation(pos).map(InvocationSt::Invocation)
            .or_from(pos)
            .or(|p| self.fn_call(p))
            .or(|p| self.return_(p).map(|_| InvocationSt::Return))
            .or(super_with)
            .into()
    }

    pub fn statement_list(&self, pos: usize) -> Step<'a, Vec<Statement<'a>>> {
        let semi = |p| self.semi(p);
        let st = |p| self.statement(p).or_none();

        seq!(pos => st, semi,).map(|sts|
            sts
                .into_iter()
                .filter_map(|s| s)
                .collect()
        )
    }
    pub fn statement(&self, pos: usize) -> Step<'a, Statement<'a>> {
        self.assignment(pos).map(Statement::Assignment)
            .or_from(pos)
            .or(|p| self.invocation_st(p).map(Statement::Invocation))
            .into()
    }


    pub fn if_st(&self, pos: usize) -> Step<'a, IfSt<'a>> {
        let th = |p| self.then(p);
        let stms = |p| self.statement_list(p);

        let if_head = |p| {
            self.if_(p)
                .then(|p| self.expression(p))
                .then_skip(th)
                .then_zip(stms)
                .map(|(cond, body)| IfBranch { cond, body })
        };

        let if_other = |p| {
            self.els_if(p)
                .then(|p| self.expression(p))
                .then_skip(th)
                .then_zip(stms)
                .map(|(cond, body)| IfBranch { cond, body })
        };

        let else_st = |p| {
            self.else_(p).then(stms)
        };

        if_head(pos)
            .then_multi_zip(if_other)
            .then_or_default_zip(else_st)
            .then_skip(|p| self.end_if(p))
            .map(|((main, others), else_body)| IfSt {
                main,
                others,
                else_body,
            })
    }
}





