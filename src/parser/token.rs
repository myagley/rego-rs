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
    UnderScore,

    As,
    Default,
    Else,
    False,
    Import,
    Not,
    Null,
    Package,
    Set,
    Some,
    True,
    With,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum StringLiteral<'input> {
    /// str needs escaping
    Escaped(&'input str),

    /// str does not need escaping
    Str(&'input str),
}

impl<'input> StringLiteral<'input> {
    // TODO: implement unescaping
    pub fn unescape(&self) -> String {
        match self {
            Self::Escaped(s) => s.to_string(),
            Self::Str(s) => s.to_string(),
        }
    }
}
