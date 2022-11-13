use parsit::test::parser_test::{expect, expect_or_env, expect_pos, fail_on};
use crate::parser::ast::case::CaseCondition;
use crate::parser::ast::numeric::{NType, Numeric, NValue};
use crate::parser::ast::{Id, SubRange, TypeCast};
use crate::parser::ast::expression::Expression;
use crate::parser::ast::statement::{AssignmentSt, AssignRhs, InvocationLine, InvocationPath, Param, InvocationSt, IfSt, IfBranch, Statement};
use crate::parser::ast::variable::{MultiVarElem, SymbolicVar, VarAccess, Variable, VarPath, VarValue};
use crate::parser::ast::variable::VarValue::{Multi, Single};
use crate::parser::tests::parser;

#[test]
fn assignment_st() {
    expect(parser("this.a := nil").assignment(0),
           AssignmentSt::JustAssign(
               Variable::Symbolic(SymbolicVar { path: VarPath::This, var: VarValue::Single(VarAccess::Value(Id("a"))) }),
               Expression::X,
           ),
    );
    expect(parser("a := null").assignment(0),
           AssignmentSt::RefAssign(
               Id("a"),
               AssignRhs::Null,
           ),
    );
}

#[test]
fn param_assign() {
    expect(parser("nil").param_assign(0), Param::Expression(Expression::X));
    expect(parser("a := nil").param_assign(0), Param::Assign(Id("a"), Expression::X));
    expect(parser("a => this.x^").param_assign(0),
           Param::OutAssign(Id("a"),
                            Variable::Symbolic(
                                SymbolicVar {
                                    path: VarPath::This,
                                    var: VarValue::Single(VarAccess::Ref(Box::new(VarAccess::Value(Id("x"))))),
                                }
                            ),
           ),
    );
    expect(parser("not a => this.x^").param_assign(0),
           Param::NotOutAssign(Id("a"),
                               Variable::Symbolic(
                                   SymbolicVar {
                                       path: VarPath::This,
                                       var: VarValue::Single(VarAccess::Ref(Box::new(VarAccess::Value(Id("x"))))),
                                   }
                               ),
           ),
    );
}

#[test]
fn param_assign_list() {
    expect(parser("(nil, a:=nil,a => this.x^)").param_assign_list(0), vec![
        Param::Expression(Expression::X),
        Param::Assign(Id("a"), Expression::X),
        Param::OutAssign(Id("a"),
                         Variable::Symbolic(
                             SymbolicVar {
                                 path: VarPath::This,
                                 var: VarValue::Single(VarAccess::Ref(Box::new(VarAccess::Value(Id("x"))))),
                             }
                         ),
        ),
    ]);

    expect(parser("()").param_assign_list(0), vec![]);
}

#[test]
fn fn_call() {
    expect(parser("a.b.c (nil, a:=nil,a => this.x^)")
               .fn_call(0),
           InvocationSt::FnCall(
               vec![Id("a"), Id("b"), Id("c")],
               vec![
                   Param::Expression(Expression::X),
                   Param::Assign(Id("a"), Expression::X),
                   Param::OutAssign(Id("a"),
                                    Variable::Symbolic(
                                        SymbolicVar {
                                            path: VarPath::This,
                                            var: VarValue::Single(VarAccess::Ref(Box::new(VarAccess::Value(Id("x"))))),
                                        }
                                    ),
                   ),
               ]));
    expect(parser("a ()")
               .fn_call(0),
           InvocationSt::FnCall(
               vec![Id("a")],
               vec![]));
}

#[test]
fn instance() {
    expect(
        parser(" this(nil, a:=nil,a => this.x^)").invocation(0),
            InvocationLine{
                path: InvocationPath::This,
                params: vec![
                    Param::Expression(Expression::X),
                    Param::Assign(Id("a"), Expression::X),
                    Param::OutAssign(Id("a"),
                                     Variable::Symbolic(
                                         SymbolicVar {
                                             path: VarPath::This,
                                             var: VarValue::Single(VarAccess::Ref(Box::new(VarAccess::Value(Id("x"))))),
                                         }
                                     ),
                    ),
                ]
            }
    );
    expect(
        parser(" x(nil, a:=nil,a => this.x^)").invocation(0),
            InvocationLine{
                path: InvocationPath::Variable(SymbolicVar{ path: VarPath::None, var: Single(VarAccess::Value(Id("x"))) }),
                params: vec![
                    Param::Expression(Expression::X),
                    Param::Assign(Id("a"), Expression::X),
                    Param::OutAssign(Id("a"),
                                     Variable::Symbolic(
                                         SymbolicVar {
                                             path: VarPath::This,
                                             var: VarValue::Single(VarAccess::Ref(Box::new(VarAccess::Value(Id("x"))))),
                                         }
                                     ),
                    ),
                ]
            }
    );
    expect(
        parser("x.y^.z.v(nil, a:=nil,a => this.x^)").invocation(0),
        InvocationLine {
            path: InvocationPath::Variable(SymbolicVar {
                path: VarPath::Path(vec![Id("x")]),
                var: Multi(VarAccess::Ref(Box::new(VarAccess::Value(Id("y")))),
                           vec![
                               MultiVarElem::Access(VarAccess::Value(Id("z"))),
                                MultiVarElem::Access(VarAccess::Value(Id("v")))
                           ]),
            }),
            params: vec![
                Param::Expression(Expression::X),
                Param::Assign(Id("a"), Expression::X),
                Param::OutAssign(Id("a"),
                                 Variable::Symbolic(
                                     SymbolicVar {
                                         path: VarPath::This,
                                         var: VarValue::Single(VarAccess::Ref(Box::new(VarAccess::Value(Id("x"))))),
                                     }
                                 ),
                ),
            ],
        },
    );
}
#[test]
fn instance_st() {
    expect(
        parser(" this(nil, a:=nil,a => this.x^)").invocation_st(0),
            InvocationSt::Invocation( InvocationLine{
                path: InvocationPath::This,
                params: vec![
                    Param::Expression(Expression::X),
                    Param::Assign(Id("a"), Expression::X),
                    Param::OutAssign(Id("a"),
                                     Variable::Symbolic(
                                         SymbolicVar {
                                             path: VarPath::This,
                                             var: VarValue::Single(VarAccess::Ref(Box::new(VarAccess::Value(Id("x"))))),
                                         }
                                     ),
                    ),
                ]
            })
    );
    expect(
        parser("return").invocation_st(0),
            InvocationSt::Return
    );
    expect(
        parser("super( )").invocation_st(0),
            InvocationSt::Super
    );
}

#[test]
fn stmt() {
    expect(
        parser("this.a := a").statement(0),
            Statement::Assignment(AssignmentSt::JustAssign(
                Variable::Symbolic(SymbolicVar { path: VarPath::This, var: VarValue::Single(VarAccess::Value(Id("a"))) }),
                Expression::X,
            ))
    )
}

#[test]
fn if_st() {
    expect(
        parser("if nil then this(nil, a:=nil,a => this.x^); return;;super(); end_if").if_st(0),
        IfSt{
            main: IfBranch { cond: Expression::X, body: vec![
                Statement::Invocation(InvocationSt::Invocation( InvocationLine{
                    path: InvocationPath::This,
                    params: vec![
                        Param::Expression(Expression::X),
                        Param::Assign(Id("a"), Expression::X),
                        Param::OutAssign(Id("a"),
                                         Variable::Symbolic(
                                             SymbolicVar {
                                                 path: VarPath::This,
                                                 var: VarValue::Single(VarAccess::Ref(Box::new(VarAccess::Value(Id("x"))))),
                                             }
                                         ),
                        ),
                    ]
                })),
                Statement::Invocation(InvocationSt::Return),
                Statement::Invocation(InvocationSt::Super),
            ] },
            others: vec![],
            else_body: vec![]
        }

    );
    expect(
        parser("if nil then this(nil, a:=nil,a => this.x^); elsif nil then return; else ; end_if").if_st(0),
        IfSt{
            main: IfBranch { cond: Expression::X, body: vec![
                Statement::Invocation(InvocationSt::Invocation( InvocationLine{
                    path: InvocationPath::This,
                    params: vec![
                        Param::Expression(Expression::X),
                        Param::Assign(Id("a"), Expression::X),
                        Param::OutAssign(Id("a"),
                                         Variable::Symbolic(
                                             SymbolicVar {
                                                 path: VarPath::This,
                                                 var: VarValue::Single(VarAccess::Ref(Box::new(VarAccess::Value(Id("x"))))),
                                             }
                                         ),
                        ),
                    ]
                })),
            ] },
            others: vec![IfBranch{
                cond: Expression::X,
                body: vec![Statement::Invocation(InvocationSt::Return)]
            }],
            else_body: vec![]
        }

    );
    expect(
        parser("if nil then this(nil, a:=nil,a => this.x^); return;;super(); else this.a := nil; return; end_if").if_st(0),
        IfSt{
            main: IfBranch { cond: Expression::X, body: vec![
                Statement::Invocation(InvocationSt::Invocation( InvocationLine{
                    path: InvocationPath::This,
                    params: vec![
                        Param::Expression(Expression::X),
                        Param::Assign(Id("a"), Expression::X),
                        Param::OutAssign(Id("a"),
                                         Variable::Symbolic(
                                             SymbolicVar {
                                                 path: VarPath::This,
                                                 var: VarValue::Single(VarAccess::Ref(Box::new(VarAccess::Value(Id("x"))))),
                                             }
                                         ),
                        ),
                    ]
                })),
                Statement::Invocation(InvocationSt::Return),
                Statement::Invocation(InvocationSt::Super),
            ] },
            others: vec![],
            else_body: vec![
                Statement::Assignment(AssignmentSt::JustAssign(
                    Variable::Symbolic(SymbolicVar { path: VarPath::This, var: VarValue::Single(VarAccess::Value(Id("a"))) }),
                    Expression::X,
                )),
                Statement::Invocation(InvocationSt::Return)
            ]
        }

    );
}
