use ordered_float::NotNan;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Token<'input> {
    StringLiteral(StringLiteral<'input>),
    IntLiteral(i64),
    FloatLiteral(NotNan<f64>),
    Identifier(&'input str),

    BraceClose,
    BraceOpen,
    BracketClose,
    BracketOpen,
    ParenClose,
    ParenOpen,

    Newline,
    CarriageReturn,

    Plus,
    Minus,
    Star,
    Slash,
    Dot,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Equal,
    EqualEqual,
    Assign,
    NotEqual,
    Colon,
    SemiColon,
    Comma,
    Amper,
    Vbar,

    As,
    Default,
    Else,
    False,
    Import,
    Not,
    Null,
    Package,
    Some,
    True,
    With,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum StringLiteral<'input> {
    Escaped(&'input str),
    Raw(&'input str),
}
