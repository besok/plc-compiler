use parsit::test::parser_test::{expect, expect_pos, fail_on};
use crate::parser::ast::case::CaseCondition;
use crate::parser::ast::numeric::{NType, Numeric, NValue};
use crate::parser::ast::{Id, SubRange, TypeCast};
use crate::parser::ast::expression::Expression;
use crate::parser::ast::variable::{MultiVarElem, SymbolicVar, VarAccess, VarPath, VarValue};
use crate::parser::tests::parser;

#[test]
fn var_access() {
    expect(parser("a").var_access(0),
           VarAccess::Value(Id("a")));
    expect(parser("a^").var_access(0),
           VarAccess::Ref(Box::new(VarAccess::Value(Id("a")))));
    expect(parser("a^^").var_access(0),
           VarAccess::Ref(Box::new(VarAccess::Ref(Box::new(VarAccess::Value(Id("a")))))),
    );
}

#[test]
fn var_value() {
    expect(parser("a").var_value(0),
           VarValue::Single(VarAccess::Value(Id("a"))),
    );
    expect(parser("a^").var_value(0),
           VarValue::Single(VarAccess::Ref(Box::new(VarAccess::Value(Id("a"))))));
    expect(parser("a^[nil,nil]").var_value(0),
           VarValue::Multi(
               VarAccess::Ref(Box::new(VarAccess::Value(Id("a")))),
               vec![MultiVarElem::Subscript(vec![Expression::X, Expression::X])],
           ));
    expect(parser("a^[nil,nil].x").var_value(0),
           VarValue::Multi(
               VarAccess::Ref(Box::new(VarAccess::Value(Id("a")))),
               vec![
                   MultiVarElem::Subscript(vec![Expression::X, Expression::X]),
                   MultiVarElem::Access(VarAccess::Value(Id("x"))),
               ],
           ));
}

#[test]
fn symbolic_var() {
    expect(parser("this.a").symbolic_var(0),
           SymbolicVar{
               path: VarPath::This,
               var: VarValue::Single(VarAccess::Value(Id("a")))
           }
    );
    expect(parser("this.a^").symbolic_var(0),
           SymbolicVar{
               path: VarPath::This,
               var: VarValue::Single(VarAccess::Ref(Box::new(VarAccess::Value(Id("a")))))
           }
    );
    expect(parser("b.a^").symbolic_var(0),
           SymbolicVar{
               path: VarPath::Path(vec![Id("b")]),
               var: VarValue::Single(VarAccess::Ref(Box::new(VarAccess::Value(Id("a")))))
           }
    );
    expect(parser("b.c.a^").symbolic_var(0),
           SymbolicVar{
               path: VarPath::Path(vec![Id("b"),Id("c")]),
               var: VarValue::Single(VarAccess::Ref(Box::new(VarAccess::Value(Id("a")))))
           }
    );
    expect(parser("b.c.a^.d^.e").symbolic_var(0),
           SymbolicVar{
               path: VarPath::Path(vec![Id("b"),Id("c")]),
               var: VarValue::Multi(VarAccess::Ref(Box::new(VarAccess::Value(Id("a")))),
                                    vec![
                                        MultiVarElem::Access(VarAccess::Ref(Box::new(VarAccess::Value(Id("d"))))),
                                        MultiVarElem::Access(VarAccess::Value(Id("e")))
                                    ])
           }
    );
    expect(parser("b.c.a^.d^.e[nil].d").symbolic_var(0),
           SymbolicVar{
               path: VarPath::Path(vec![Id("b"),Id("c")]),
               var: VarValue::Multi(VarAccess::Ref(Box::new(VarAccess::Value(Id("a")))),
                                    vec![
                                        MultiVarElem::Access(VarAccess::Ref(Box::new(VarAccess::Value(Id("d"))))),
                                        MultiVarElem::Access(VarAccess::Value(Id("e"))),
                                        MultiVarElem::Subscript(vec![Expression::X]),
                                        MultiVarElem::Access(VarAccess::Value(Id("d")))
                                    ])
           }
    );

}