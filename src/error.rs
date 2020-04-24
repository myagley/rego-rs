use std::fmt;

use serde::{de, ser};

use crate::lexer::Error as LexerError;
use crate::value::Value;

#[derive(Debug, Clone)]
pub enum Error {
    InvalidType(&'static str, Value),
    Lexer(LexerError),
    SerializeValue(String),
    DeserializeValue(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::InvalidType(s, v) => write!(
                f,
                "Invalid type in builtin function: expected {}, got {:?}",
                s, v
            ),
            Error::Lexer(_) => write!(f, "The lexer encountered an error."),
            Error::SerializeValue(s) => write!(f, "Failed to serialize value: {}", s),
            Error::DeserializeValue(s) => write!(f, "Failed to deserialize value: {}", s),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Lexer(ref e) => Some(e),
            _ => None,
        }
    }
}

impl From<LexerError> for Error {
    fn from(e: LexerError) -> Self {
        Self::Lexer(e)
    }
}

impl de::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Error {
        Error::DeserializeValue(msg.to_string())
    }
}

impl ser::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Error {
        Error::SerializeValue(msg.to_string())
    }
}
