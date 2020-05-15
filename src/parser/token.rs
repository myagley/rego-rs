use std::fmt;

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

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Token::*;
        match self {
            StringLiteral(s) => write!(f, "\"{}\"", s),
            IntLiteral(i) => write!(f, "{}", i),
            FloatLiteral(float) => write!(f, "{}", float),
            Identifier(id) => write!(f, "{}", id),

            BraceClose => write!(f, "}}"),
            BraceOpen => write!(f, "{{"),
            BracketClose => write!(f, "]"),
            BracketOpen => write!(f, "["),
            ParenClose => write!(f, ")"),
            ParenOpen => write!(f, "("),

            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Star => write!(f, "*"),
            Slash => write!(f, "/"),
            Dot => write!(f, "."),
            Greater => write!(f, ">"),
            GreaterEqual => write!(f, ">="),
            Less => write!(f, "<"),
            LessEqual => write!(f, "<="),
            Equal => write!(f, "="),
            EqualEqual => write!(f, "=="),
            Assign => write!(f, ":="),
            NotEqual => write!(f, "!="),
            Colon => write!(f, ":"),
            SemiColon => write!(f, ";"),
            Comma => write!(f, ","),
            Amper => write!(f, "&"),
            Vbar => write!(f, "|"),
            UnderScore => write!(f, "_"),

            As => write!(f, "as"),
            Default => write!(f, "display"),
            Else => write!(f, "else"),
            False => write!(f, "false"),
            Import => write!(f, "import"),
            Not => write!(f, "not"),
            Null => write!(f, "null"),
            Package => write!(f, "package"),
            Set => write!(f, "set"),
            Some => write!(f, "some"),
            True => write!(f, "true"),
            With => write!(f, "with"),
        }
    }
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

impl fmt::Display for StringLiteral<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StringLiteral::Escaped(s) => write!(f, "{}", s),
            StringLiteral::Str(s) => write!(f, "{}", s),
        }
    }
}
