use logos::{FilterResult, Lexer, Logos};
use logos::skip;
use crate::parser::ast::{Text, to_text, to_type_cast, TypeCast};
use crate::parser::ast::numeric::Numeric;
use crate::parser::ast::numeric::to_number;
use crate::parser::ast::numeric::to_real;
use crate::parser::ast::time::{Date, to_date_value, to_tod_value, to_data_time_value, to_duration};

#[derive(Logos, Clone, Copy, Debug, PartialEq)]
#[logos(subpattern int_type = r"(?i)(USINT|UINT|UDINT|ULINT|SINT|INT|DINT|LINT)#")]
#[logos(subpattern bit_type = r"(?i)(BOOL|BYTE|WORD|DWORD|LWORD|DW|B|W)#")]
#[logos(subpattern real_type = r"(?i)(REAL|LREAL)#")]
#[logos(subpattern base = r"(2|8|16)#")]
#[logos(subpattern digit = r"[-+]?[0-9][0-9_]*")]
#[logos(subpattern u_digit = r"[0-9][0-9_]*")]
#[logos(subpattern hex = r"16#[-+]?[0-9_a-fA-F]+")]
#[logos(subpattern oct = r"8#[-+]?[0-7_]+")]
#[logos(subpattern bin = r"2#[-+]?[01_]+")]
#[logos(subpattern id = r"([a-zA-Z_][$a-zA-Z0-9_]*)|`([a-zA-Z_][$a-zA-Z0-9_]*)`")]
#[logos(subpattern exp = r"[eE][+-]?[0-9]+")]
pub enum Token<'a> {
    #[regex(r"(?&id)")]
    Id(&'a str),

    #[regex(r"((?&int_type)|(?&bit_type))?((?&digit)|(?&hex)|(?&oct)|(?&bin))", to_number)]
    #[regex(r"((?&real_type))?(?&digit)?\.(?&u_digit)(?&exp)?", to_real)]
    Numeric(Numeric),

    #[regex("\"((\\$.)|[^$\"])*\"", to_text)]
    #[regex("'((\\$.)|[^$'])*'", to_text)]
    Text(Text<'a>),

    #[regex("(LTIME|LT|TIME|T)#-?(\\d+(\\.\\d+)?(d|h|ms|m|s|us|ns))+", to_duration, ignore(case))]
    #[regex("(LDATE|DATE|LD|D)#\\d+-\\d+-\\d+", to_date_value, ignore(case))]
    #[regex("(DATE_AND_TIME|DT|LDATE_AND_TIME|LDT)#\\d+-\\d+-\\d+-\\d+:\\d+(:\\d+(\\.\\d+)?)?", to_data_time_value, ignore(case))]
    #[regex("(TIME_OF_DAY|LTIME_OF_DAY|TOD|LTOD)#\\d+:\\d+(:\\d+(\\.\\d+)?)?", to_tod_value, ignore(case))]
    DateTime(Date),

    #[regex("(?&id)#(?&id)", to_type_cast)]
    TypeCastPrefix(TypeCast<'a>),
    #[regex("%[IQM][XBWDL]?[0-9]+(\\.[0-9]+)?", ignore(case))]
    DirectVariable(&'a str),

    #[token("ANY", ignore(case))]
    Any,
    #[token("ANY_BIT", ignore(case))]
    AnyBit,
    #[token("ANY_DATE", ignore(case))]
    AnyDate,
    #[token("ANY_DERIVED", ignore(case))]
    AnyDerived,
    #[token("ANY_ELEMENTARY", ignore(case))]
    AnyElementary,
    #[token("ANY_INT", ignore(case))]
    AnyInt,
    #[token("ANY_MAGNITUDE", ignore(case))]
    AnyMag,

    #[token("ANY_NUM", ignore(case))]
    AnyNum,

    #[token("ANY_REAL", ignore(case))]
    AnyReal,
    #[token("ANY_STRING", ignore(case))]
    AnyString,

    #[token("PROGRAM", ignore(case))]
    Program,

    #[token("READ_ONLY", ignore(case))]
    ReadOnly,

    #[token("READ_WRITE", ignore(case))]
    ReadWrite,

    #[token("CLASS", ignore(case))]
    Class,

    #[token("END_CLASS", ignore(case))]
    EndClass,

    #[token("VAR_INPUT", ignore(case))]
    VarInput,

    #[token("VAR_OUTPUT", ignore(case))]
    VarOutput,

    #[token("VAR", ignore(case))]
    Var,

    #[token("VAR_ACCESS", ignore(case))]
    VarAccess,
    #[token("VAR_CONFIG", ignore(case))]
    VarConfig,

    #[token("VAR_EXTERNAL", ignore(case))]
    VarExternal,

    #[token("ABSTRACT", ignore(case))]
    Abstract,

    #[token("IMPLEMENTS", ignore(case))]
    Implements,

    #[token("SUPER", ignore(case))]
    Super,

    #[token("THIS", ignore(case))]
    This,

    #[token("EXTENDS", ignore(case))]
    Extends,

    #[token("FINAL", ignore(case))]
    Final,

    #[token("METHOD", ignore(case))]
    Method,

    #[token("END_METHOD", ignore(case))]
    EndMethod,

    #[token("INTERFACE", ignore(case))]
    Interface,

    #[token("END_INTERFACE", ignore(case))]
    EndInterface,

    #[token("CONSTANT", ignore(case))]
    Constant,

    #[token("RETAIN", ignore(case))]
    Retain,

    #[token("NON_RETAIN", ignore(case))]
    NonRetain,

    #[token("VAR_TEMP", ignore(case))]
    VarTemp,

    #[token("PUBLIC", ignore(case))]
    Public,

    #[token("PRIVATE", ignore(case))]
    Private,

    #[token("INTERNAL", ignore(case))]
    Internal,

    #[token("PROTECTED", ignore(case))]
    Protected,

    #[token("OVERRIDE", ignore(case))]
    Override,

    #[token("VAR_GLOBAL", ignore(case))]
    VarGlobal,

    #[token("VAR_IN_OUT", ignore(case))]
    VarInOut,

    #[token("END_VAR", ignore(case))]
    EndVar,

    #[token("END_PROGRAM", ignore(case))]
    EndProgram,

    #[token("FUNCTION", ignore(case))]
    Function,
    #[token("F_EDGE", ignore(case))]
    FEdge,
    #[token("ON", ignore(case))]
    On,

    #[token("STEP", ignore(case))]
    Step,

    #[token("END_STEP", ignore(case))]
    EndStep,
    #[token("FROM", ignore(case))]
    From,
    #[token("END_TRANSITION", ignore(case))]
    EndTransition,
    #[token("TRANSITION", ignore(case))]
    Transition,

    #[token("INITIAL_STEP", ignore(case))]
    InitStep,

    #[token("END_FUNCTION", ignore(case))]
    EndFunction,

    #[token("FUNCTION_BLOCK", ignore(case))]
    FunctionBlock,

    #[token("END_FUNCTION_BLOCK", ignore(case))]
    EndFunctionBlock,

    #[token("TYPE", ignore(case))]
    Type,
    #[token("END_TYPE", ignore(case))]
    EndType,

    #[token("STRUCT", ignore(case))]
    Struct,

    #[token("TASK", ignore(case))]
    Task,

    #[token("END_STRUCT", ignore(case))]
    EndStruct,

    #[token("ACTIONS", ignore(case))]
    Actions,
    #[token("END_ACTIONS", ignore(case))]
    EndActions,

    #[token("ACTION", ignore(case))]
    Action,

    #[token("END_ACTION", ignore(case))]
    EndAction,

    #[token(":")]
    Colon,

    #[token(";")]
    Semi,

    #[token(":=")]
    Assign,

    #[token("=>")]
    OutAssign,

    #[token("REF=")]
    RAssign,

    #[token("?=")]
    OptAssign,

    #[token("MOD", ignore(case))]
    Mod,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LBrack,

    #[token("$")]
    Dollar,

    #[token("]")]
    RBrack,

    #[token(",")]
    Comma,

    #[token("...")]
    EllipsisOut,

    #[token("..")]
    EllipsisIn,

    #[token(".")]
    Dot,

    #[token("IF", ignore(case))]
    If,
    #[token("INTERVAL", ignore(case))]
    Interval,
    #[token("JMP", ignore(case))]
    #[token("Goto", ignore(case))]
    Goto,

    #[token("THEN", ignore(case))]
    Then,

    #[token("ELSIF", ignore(case))]
    #[token("ELSE IF", ignore(case))]
    ElseIf,

    #[token("ELSE", ignore(case))]
    Else,

    #[token("END_IF", ignore(case))]
    EndIf,

    #[token("FOR", ignore(case))]
    For,

    #[token("TO", ignore(case))]
    To,

    #[token("BY", ignore(case))]
    By,

    #[token("DO", ignore(case))]
    Do,

    #[token("END_FOR", ignore(case))]
    EndFor,

    #[token("WHILE", ignore(case))]
    While,

    #[token("WITH", ignore(case))]
    With,

    #[token("END_WHILE", ignore(case))]
    EndWhile,

    #[token("REPEAT", ignore(case))]
    Repeat,

    #[token("END_REPEAT", ignore(case))]
    EndRepeat,

    #[token("END_RESOURCE", ignore(case))]
    EndResource,

    #[token("RESOURCE", ignore(case))]
    Resource,

    #[token("UNTIL", ignore(case))]
    Until,

    #[token("CASE", ignore(case))]
    Case,
    #[token("CONFIGURATION", ignore(case))]
    Configuration,

    #[token("END_CONFIGURATION", ignore(case))]
    EndConfiguration,

    #[token("END_CASE", ignore(case))]
    EndCase,

    #[token("RETURN", ignore(case))]
    Return,

    #[token("R_EDGE", ignore(case))]
    REdge,

    #[token("SINGLE", ignore(case))]
    Single,

    #[token("EXIT", ignore(case))]
    Exit,

    #[token("CONTINUE", ignore(case))]
    Continue,

    #[token("POINTER", ignore(case))]
    Pointer,

    #[token("REF_TO", ignore(case))]
    RefTo,
    #[token("REF", ignore(case))]
    Ref,

    #[token("ARRAY", ignore(case))]
    Array,

    #[token("BOOL", ignore(case))]
    Bool,
    #[token("BYTE", ignore(case))]
    Byte,

    #[token("DATE_AND_TIME", ignore(case))]
    DateAndTime,
    #[token("DATE", ignore(case))]
    Date,
    #[token("DT", ignore(case))]
    DT,

    #[token("STRING", ignore(case))]
    String,
    #[token("INT", ignore(case))]
    Int,
    #[token("DINT", ignore(case))]
    DInt,
    #[token("LINT", ignore(case))]
    LInt,
    #[token("LREAL", ignore(case))]
    LReal,
    #[token("LWORD", ignore(case))]
    LWord,
    #[token("WORD", ignore(case))]
    Word,
    #[token("REAL", ignore(case))]
    Real,
    #[token("SINT", ignore(case))]
    SInt,

    #[token("WSTRING", ignore(case))]
    WString,
    #[token("TIME", ignore(case))]
    Time,
    #[token("TIME_OF_DAY", ignore(case))]
    #[token("TOD", ignore(case))]
    TimeOfDay,
    #[token("DWORD", ignore(case))]
    DWord,

    #[token("NAMESPACE", ignore(case))]
    Namespace,

    #[token("USING", ignore(case))]
    Using,

    #[token("PERSISTENT", ignore(case))]
    Persistent,

    #[token("END_NAMESPACE", ignore(case))]
    EndNamespace,

    #[token("UDINT", ignore(case))]
    UDInt,
    #[token("UINT", ignore(case))]
    UInt,
    #[token("ULINT", ignore(case))]
    ULInt,

    #[token("USINT", ignore(case))]
    USInt,

    #[token("OF", ignore(case))]
    Of,

    #[token("PRIORITY", ignore(case))]
    Priority,

    #[token("AT", ignore(case))]
    At,

    #[token("+")]
    Plus,

    #[token("_")]
    Under,

    #[token("-")]
    Minus,

    #[token("*")]
    Multi,

    #[token("**")]
    Power,
    #[token("+=")]
    Increase,
    #[token("-=")]
    Decrease,

    #[token("/")]
    Div,

    #[token("=")]
    Eq,

    #[token("<>")]
    NotEq,

    #[token(">")]
    Gt,
    #[token(">=")]
    Ge,
    #[token("<")]
    Lt,
    #[token("<=")]
    Le,

    #[token("&")]
    Amp,

    #[token("^")]
    Caret,

    #[token("::")]
    DColon,

    #[token("->")]
    RArrow,

    #[token("AND", ignore(case))]
    And,

    #[token("OR", ignore(case))]
    Or,

    #[token("XOR", ignore(case))]
    XOr,

    #[token("NOT", ignore(case))]
    Not,
    #[token("NULL", ignore(case))]
    Null,
    #[token("NIL", ignore(case))]
    Nil,

    #[token("TRUE", ignore(case))]
    True,

    #[token("FALSE", ignore(case))]
    False,

    #[regex(r"\{[^\r\n}{]*}", skip)]
    Pragma,

    #[regex(r"\(\*[^\r\n*)(]*\*\)", skip)]
    #[regex(r"\\\\[^\r\n]*", skip)]
    #[regex(r#"//\*.*\*//"#, logos::skip)]
    Comment,

    #[regex(r"[ \t\u000C\r\n]+", skip)]
    WS,

    #[error]
    Error,
}


#[cfg(test)]
mod tests {
    use parsit::test::lexer_test as lt;
    use crate::parser::ast::numeric::{NType, Numeric, NValue};
    use crate::parser::ast::Text;
    use crate::parser::ast::time::{Date, DateValue, DurationValue, TimeValue};
    use crate::parser::tokens::Token;

    #[test]
    fn script(){
        lt::expect_succeed::<Token>(r#"
TYPE MyStruct: STRUCT  x: DINT; y: DINT; END_STRUCT END_TYPE
FUNCTION main : DINT
	main := foo();
END_FUNCTION

FUNCTION foo : DINT
VAR
				x : DINT;
				s : MyStruct;
				u,y : REF_TO DINT;
				z : REF_TO REF_TO DINT;

END_VAR
u := NULL;
u := &s.x;
y := u;
z := &y;
s.x := 9;
z^^ := y^*2;
y^ := z^^*2;

foo := y^;
END_FUNCTION
        "#)
    }

    #[test]
    fn comments() {
        lt::expect_succeed::<Token>("\\\\ some text)");
        lt::expect_succeed::<Token>("(* text *)");
        lt::expect_failed::<Token>("(* (* text *)*)");
        lt::expect_failed::<Token>("(* * text *)*)");
        lt::expect_succeed::<Token>("(* text *)");
        lt::expect_succeed::<Token>("(**)");
        lt::expect_succeed::<Token>("/* text */");
        lt::expect_succeed::<Token>(r#"
        (**)
        (*a*)
        (*aa*)
        (*aaaa*)
        "#);
    }

    #[test]
    fn pragma() {
        lt::expect::<Token>(r#"
        (**)
        { some text }
        (*aa*)
        { another = (* x *)}
        "#, vec![]);

        lt::expect_failed::<Token>("{ some= text");
        lt::expect_failed::<Token>(r"{ some= text
        }")
    }

    #[test]
    fn number() {
        lt::expect_succeed::<Token>("1");
        lt::expect_succeed::<Token>("1_1");
        lt::expect_succeed::<Token>("1_1010");
        lt::expect_succeed::<Token>("11010");
        lt::expect_succeed::<Token>("2#11010");
        lt::expect_succeed::<Token>("8#7_7");
        lt::expect_succeed::<Token>("16#ff");
        lt::expect_succeed::<Token>("b#16#fff");
        lt::expect_succeed::<Token>("uint#16#fff");
        lt::expect::<Token>("Real#1.0", vec![Token::Numeric(
            Numeric::Typed(NType::REAL, NValue::Real(1.0))
        )]);
        lt::expect::<Token>("1.0", vec![Token::Numeric(
            Numeric::UnTyped(NValue::Real(1.0))
        )]);
    }

    #[test]
    fn id() {
        lt::expect_succeed::<Token>("abc");
        lt::expect_succeed::<Token>("_abc");
        lt::expect_succeed::<Token>("_a$bc_0");
        lt::expect_succeed::<Token>("_1");
        lt::expect_succeed::<Token>("`abc`");
        lt::expect_succeed::<Token>("`_a_$_bc`");
    }

    #[test]
    fn text() {
        lt::expect::<Token>(r#""abc""#, vec![Token::Text(Text("abc"))]);
        lt::expect_succeed::<Token>(r#""""#);
        lt::expect_succeed::<Token>(r#"'abc'"#);
        lt::expect_succeed::<Token>(r#"''"#);
        lt::expect_succeed::<Token>(r#"'$$abc'"#);
        lt::expect_succeed::<Token>(r#"'$'abc$''"#);
        lt::expect_succeed::<Token>(r#""
          some text
        ""#);
    }

    #[test]
    fn type_cast() {
        lt::expect_succeed::<Token>(r#"a#b"#);
    }

    #[test]
    fn direct_var() {
        lt::expect_succeed::<Token>("%IX1.1 %IB2.2 %QW5 %MD7");
    }

    #[test]
    fn date() {
        lt::expect::<Token>(r#"DATE#1984-10-01"#,
                            vec![Token::DateTime(Date::Date(DateValue { year: 1984, month: 10, day: 1 }))],
        );
        lt::expect::<Token>(r#"D#1-1-1"#,
                            vec![Token::DateTime(Date::Date(DateValue { year: 1, month: 1, day: 1 }))],
        );
        lt::expect::<Token>(r#"D#2001-10-04"#,
                            vec![Token::DateTime(Date::Date(DateValue { year: 2001, month: 10, day: 4 }))],
        );
        lt::expect::<Token>(r#"D#1-1-1"#,
                            vec![Token::DateTime(Date::Date(DateValue { year: 1, month: 1, day: 1 }))],
        );
        // TOD#1:1:1 TOD#1:1:1.123 TIME_OF_DAY#12:13 TOD#10:20
        lt::expect::<Token>(r#"TIME_OF_DAY#20:15:12"#,
                            vec![Token::DateTime(Date::Time(TimeValue { hour: 20, minute: 15, second: 12, nano: 0 }))],
        );
        lt::expect::<Token>(r#"TOD#1:1:1.123"#,
                            vec![Token::DateTime(Date::Time(TimeValue { hour: 1, minute: 1, second: 1, nano: 123000000 }))],
        );
        lt::expect::<Token>(r#"TIME_OF_DAY#12:13"#,
                            vec![Token::DateTime(Date::Time(TimeValue { hour: 12, minute: 13, second: 0, nano: 0 }))],
        );
        //    DATE_AND_TIME#2000-01-01-20:15
        lt::expect::<Token>(r#"DATE_AND_TIME#1984-10-01-20:15:12"#,
                            vec![Token::DateTime(Date::DateTime(DateValue { year: 1984, month: 10, day: 1 }, TimeValue { hour: 20, minute: 15, second: 12, nano: 0 }))],
        );
        lt::expect::<Token>(r#"DT#1-1-1-1:1:1"#,
                            vec![Token::DateTime(Date::DateTime(DateValue { year: 1, month: 1, day: 1 }, TimeValue { hour: 1, minute: 1, second: 1, nano: 0 }))],
        );

        lt::expect::<Token>(r#"DT#1-1-1-1:1:1.123"#,
                            vec![Token::DateTime(Date::DateTime(DateValue { year: 1, month: 1, day: 1 }, TimeValue { hour: 1, minute: 1, second: 1, nano: 123000000 }))],
        );
        lt::expect::<Token>(r#"DATE_AND_TIME#2000-01-01-20:15"#,
                            vec![Token::DateTime(Date::DateTime(DateValue { year: 2000, month: 1, day: 1 }, TimeValue { hour: 20, minute: 15, second: 0, nano: 0 }))],
        );

        lt::expect::<Token>(r#"T#12d10ms"#,
                            vec![Token::DateTime(Date::Duration(DurationValue{ day: 12.0, hour: 0.0, min: 0.0, sec: 0.0, milli: 10.0, micro: 0.0, nano: 0, negative: false }))],
        );
        lt::expect::<Token>(r#"T#12d10m"#,
                            vec![Token::DateTime(Date::Duration(DurationValue{ day: 12.0, hour: 0.0, min: 10.0, sec: 0.0, milli: 0.0, micro: 0.0, nano: 0, negative: false }))],
        );
        lt::expect::<Token>(r#"TIME#12m4s3ns"#,
                            vec![Token::DateTime(Date::Duration(DurationValue{ day: 0.0, hour: 0.0, min: 12.0, sec: 4.0, milli: 0.0, micro: 0.0, nano: 3, negative: false }))],
        );
        lt::expect::<Token>(r#"TIME#4d6h8m7s12ms04us2ns"#,
                            vec![Token::DateTime(Date::Duration(DurationValue{ day: 4.0, hour: 6.0, min: 8.0, sec: 7.0, milli: 12.0, micro: 04.0, nano: 2, negative: false }))],
        );
    }
}


