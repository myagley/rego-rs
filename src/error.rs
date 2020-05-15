use std::fmt;

use serde::{de, ser};

use crate::parser::lexer::Error as LexerError;
use crate::parser::ParseError;
use crate::{ast, value, vm};

#[derive(Debug, Clone)]
pub struct Error<'input> {
    kind: ErrorKind<'input>,
}

impl fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl<'input> std::error::Error for Error<'input> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self.kind {
            ErrorKind::ValueConversion(ref e) => Some(e),
            ErrorKind::Lexer(ref e) => Some(e),
            ErrorKind::Semantic(ref e) => Some(e),
            ErrorKind::Runtime(ref e) => Some(e),
            _ => None,
        }
    }
}

impl<'a> From<ErrorKind<'a>> for Error<'a> {
    fn from(kind: ErrorKind<'a>) -> Self {
        Error { kind }
    }
}

#[derive(Debug, Clone)]
enum ErrorKind<'input> {
    ValueConversion(value::InvalidType),
    Lexer(LexerError),
    Parser(ParseError<'input>),
    Semantic(ast::Error),
    Runtime(vm::Error),
    SerializeValue(String),
    DeserializeValue(String),
}

impl fmt::Display for ErrorKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorKind::ValueConversion(_) => write!(f, "Failed to convert value."),
            ErrorKind::Lexer(_) => write!(f, "The lexer encountered an error."),
            ErrorKind::Parser(e) => write!(f, "The parser encountered an error: {}", e),
            ErrorKind::Semantic(_) => write!(f, "An error occurred analyzing the query."),
            ErrorKind::Runtime(_) => write!(f, "An error occurred while executing the query."),
            ErrorKind::SerializeValue(s) => write!(f, "Failed to serialize value: {}", s),
            ErrorKind::DeserializeValue(s) => write!(f, "Failed to deserialize value: {}", s),
        }
    }
}

impl From<value::InvalidType> for Error<'_> {
    fn from(e: value::InvalidType) -> Self {
        ErrorKind::ValueConversion(e).into()
    }
}

impl From<LexerError> for Error<'_> {
    fn from(e: LexerError) -> Self {
        ErrorKind::Lexer(e).into()
    }
}

impl<'a> From<ParseError<'a>> for Error<'a> {
    fn from(e: ParseError<'a>) -> Self {
        ErrorKind::Parser(e).into()
    }
}

impl From<ast::Error> for Error<'_> {
    fn from(e: ast::Error) -> Self {
        ErrorKind::Semantic(e).into()
    }
}

impl From<vm::Error> for Error<'_> {
    fn from(e: vm::Error) -> Self {
        ErrorKind::Runtime(e).into()
    }
}

impl<'a> de::Error for Error<'a> {
    fn custom<T: fmt::Display>(msg: T) -> Error<'a> {
        ErrorKind::DeserializeValue(msg.to_string()).into()
    }
}

impl<'a> ser::Error for Error<'a> {
    fn custom<T: fmt::Display>(msg: T) -> Error<'a> {
        ErrorKind::SerializeValue(msg.to_string()).into()
    }
}
