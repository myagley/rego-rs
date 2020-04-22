#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Token<'input> {
    Null,
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
    Assign,
    Bang,
    NotEqual,
    EqualEqual,
    Colon,
    SemiColon,
    Comma,
    Amper,
    Vbar,
    Newline,
    CarriageReturn,
    StringLiteral(StringLiteral<'input>),
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum StringLiteral<'input> {
    Escaped(&'input str),
    Raw(&'input str),
}
