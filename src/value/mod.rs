use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::ops::{Add, Div, Mul, Sub};

mod from;
mod index;
mod number;

use crate::error::Error;

pub use self::index::Index;
pub use self::number::Number;

pub type Map<K, V> = BTreeMap<K, V>;
pub type Set<V> = BTreeSet<V>;

#[derive(Clone, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub enum Value {
    Undefined,
    Null,
    Bool(bool),
    Number(Number),
    String(String),
    Array(Vec<Value>),
    Object(Map<Value, Value>),
    Set(Set<Value>),
}

impl fmt::Debug for Value {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Undefined => formatter.debug_tuple("Undefined").finish(),
            Value::Null => formatter.debug_tuple("Null").finish(),
            Value::Bool(v) => formatter.debug_tuple("Bool").field(&v).finish(),
            Value::Number(ref v) => fmt::Debug::fmt(v, formatter),
            Value::String(ref v) => formatter.debug_tuple("String").field(v).finish(),
            Value::Array(ref v) => formatter.debug_tuple("Array").field(v).finish(),
            Value::Object(ref v) => formatter.debug_tuple("Object").field(v).finish(),
            Value::Set(ref v) => formatter.debug_tuple("Set").field(v).finish(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Undefined => write!(f, "undefined"),
            Value::Null => write!(f, "null"),
            Value::Bool(ref v) => fmt::Display::fmt(v, f),
            Value::Number(ref v) => fmt::Display::fmt(v, f),
            Value::String(ref v) => write!(f, "\"{}\"", v.escape_default()),
            Value::Array(ref v) => {
                write!(f, "[")?;
                let mut iter = v.iter();
                if let Some(first) = iter.next() {
                    fmt::Display::fmt(first, f)?;
                }
                while let Some(elem) = iter.next() {
                    write!(f, ",")?;
                    fmt::Display::fmt(elem, f)?;
                }
                write!(f, "]")
            }
            Value::Object(ref v) => {
                write!(f, "{{")?;
                let mut iter = v.iter();
                if let Some((k, v)) = iter.next() {
                    fmt::Display::fmt(k, f)?;
                    write!(f, ":")?;
                    fmt::Display::fmt(v, f)?;
                }
                while let Some((k, v)) = iter.next() {
                    write!(f, ",")?;
                    fmt::Display::fmt(k, f)?;
                    write!(f, ":")?;
                    fmt::Display::fmt(v, f)?;
                }
                write!(f, "}}")
            }
            Value::Set(ref v) => {
                write!(f, "{{")?;
                let mut iter = v.iter();
                if let Some(first) = iter.next() {
                    fmt::Display::fmt(first, f)?;
                }
                while let Some(elem) = iter.next() {
                    write!(f, ",")?;
                    fmt::Display::fmt(elem, f)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl Default for Value {
    fn default() -> Value {
        Value::Null
    }
}

impl Value {
    pub fn get<I: Index>(&self, index: I) -> Option<&Value> {
        index.index_into(self)
    }

    pub fn get_mut<I: Index>(&mut self, index: I) -> Option<&mut Value> {
        index.index_into_mut(self)
    }

    pub fn try_into_set(self) -> Result<Set<Value>, Error> {
        match self {
            Value::Set(v) => Ok(v),
            v => Err(Error::InvalidType("set", Type(&v).ty())),
        }
    }

    pub fn as_set(&self) -> Option<&Set<Value>> {
        match *self {
            Value::Set(ref set) => Some(set),
            _ => None,
        }
    }

    pub fn as_set_mut(&mut self) -> Option<&mut Set<Value>> {
        match *self {
            Value::Set(ref mut set) => Some(set),
            _ => None,
        }
    }

    pub fn is_set(&self) -> bool {
        self.as_set().is_some()
    }

    pub fn try_into_object(self) -> Result<Map<Value, Value>, Error> {
        match self {
            Value::Object(map) => Ok(map),
            v => Err(Error::InvalidType("object", Type(&v).ty())),
        }
    }

    pub fn as_object(&self) -> Option<&Map<Value, Value>> {
        match *self {
            Value::Object(ref map) => Some(map),
            _ => None,
        }
    }

    pub fn as_object_mut(&mut self) -> Option<&mut Map<Value, Value>> {
        match *self {
            Value::Object(ref mut map) => Some(map),
            _ => None,
        }
    }

    pub fn is_object(&self) -> bool {
        self.as_object().is_some()
    }

    pub fn try_into_array(self) -> Result<Vec<Value>, Error> {
        match self {
            Value::Array(array) => Ok(array),
            v => Err(Error::InvalidType("array", Type(&v).ty())),
        }
    }

    pub fn as_array(&self) -> Option<&Vec<Value>> {
        match *self {
            Value::Array(ref array) => Some(array),
            _ => None,
        }
    }

    pub fn as_array_mut(&mut self) -> Option<&mut Vec<Value>> {
        match *self {
            Value::Array(ref mut array) => Some(array),
            _ => None,
        }
    }

    pub fn is_array(&self) -> bool {
        self.as_array().is_some()
    }

    pub fn try_into_string(self) -> Result<String, Error> {
        match self {
            Value::String(string) => Ok(string),
            v => Err(Error::InvalidType("string", Type(&v).ty())),
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match *self {
            Value::String(ref string) => Some(string),
            _ => None,
        }
    }

    pub fn is_string(&self) -> bool {
        self.as_str().is_some()
    }

    pub fn is_number(&self) -> bool {
        match *self {
            Value::Number(_) => true,
            _ => false,
        }
    }

    pub fn try_into_i64(self) -> Result<i64, Error> {
        match self {
            Value::Number(n) => n.try_into_i64(),
            v => Err(Error::InvalidType("i64", Type(&v).ty())),
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match *self {
            Value::Number(ref n) => n.as_i64(),
            _ => None,
        }
    }

    pub fn is_i64(&self) -> bool {
        match *self {
            Value::Number(ref n) => n.is_i64(),
            _ => false,
        }
    }

    pub fn try_into_u64(self) -> Result<u64, Error> {
        match self {
            Value::Number(n) => n.try_into_u64(),
            v => Err(Error::InvalidType("u64", Type(&v).ty())),
        }
    }

    pub fn as_u64(&self) -> Option<u64> {
        match *self {
            Value::Number(ref n) => n.as_u64(),
            _ => None,
        }
    }

    pub fn is_u64(&self) -> bool {
        match *self {
            Value::Number(ref n) => n.is_u64(),
            _ => false,
        }
    }

    pub fn try_into_f64(self) -> Result<f64, Error> {
        match self {
            Value::Number(n) => n.try_into_f64(),
            v => Err(Error::InvalidType("f64", Type(&v).ty())),
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        match *self {
            Value::Number(ref n) => n.as_f64(),
            _ => None,
        }
    }

    pub fn is_f64(&self) -> bool {
        match *self {
            Value::Number(ref n) => n.is_f64(),
            _ => false,
        }
    }

    pub fn try_into_bool(self) -> Result<bool, Error> {
        match self {
            Value::Bool(b) => Ok(b),
            v => Err(Error::InvalidType("bool", Type(&v).ty())),
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match *self {
            Value::Bool(b) => Some(b),
            _ => None,
        }
    }

    pub fn is_boolean(&self) -> bool {
        self.as_bool().is_some()
    }

    pub fn as_null(&self) -> Option<()> {
        match *self {
            Value::Null => Some(()),
            _ => None,
        }
    }

    pub fn is_null(&self) -> bool {
        self.as_null().is_some()
    }

    pub fn is_undefined(&self) -> bool {
        matches!(*self, Value::Undefined)
    }
}

struct Type<'a>(&'a Value);

impl<'a> Type<'a> {
    pub fn ty(&self) -> &'static str {
        match *self.0 {
            Value::Undefined => "undefined",
            Value::Null => "null",
            Value::Bool(_) => "bool",
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Array(_) => "array",
            Value::Object(_) => "object",
            Value::Set(_) => "set",
        }
    }
}

impl<'a> fmt::Display for Type<'a> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match *self.0 {
            Value::Undefined => formatter.write_str("undefined"),
            Value::Null => formatter.write_str("null"),
            Value::Bool(_) => formatter.write_str("bool"),
            Value::Number(_) => formatter.write_str("number"),
            Value::String(_) => formatter.write_str("string"),
            Value::Array(_) => formatter.write_str("array"),
            Value::Object(_) => formatter.write_str("object"),
            Value::Set(_) => formatter.write_str("set"),
        }
    }
}

macro_rules! impl_binop {
    (impl $imp:ident, $method:ident) => {
        impl $imp for Value {
            type Output = Value;

            fn $method(self, other: Self) -> Self::Output {
                match (self, other) {
                    (Value::Number(l), Value::Number(r)) => Value::Number($imp::$method(l, r)),
                    _ => Value::Undefined,
                }
            }
        }

        impl<'v> $imp<Value> for &'v Value {
            type Output = Value;

            fn $method(self, other: Value) -> Self::Output {
                match (self, other) {
                    (Value::Number(ref l), Value::Number(r)) => Value::Number($imp::$method(*l, r)),
                    _ => Value::Undefined,
                }
            }
        }

        impl $imp<&Value> for Value {
            type Output = Value;

            fn $method(self, other: &Self) -> Self::Output {
                match (self, other) {
                    (Value::Number(l), Value::Number(ref r)) => Value::Number($imp::$method(l, *r)),
                    _ => Value::Undefined,
                }
            }
        }

        impl<'v, 'a: 'v> $imp<&'a Value> for &'v Value {
            type Output = Value;

            fn $method(self, other: Self) -> Self::Output {
                match (self, other) {
                    (Value::Number(ref l), Value::Number(ref r)) => {
                        Value::Number($imp::$method(l, *r))
                    }
                    _ => Value::Undefined,
                }
            }
        }
    };
}

impl_binop!(impl Add, add);
impl_binop!(impl Sub, sub);
impl_binop!(impl Mul, mul);
impl_binop!(impl Div, div);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoketest_addition() {
        assert_eq!(
            Value::from(Number::from(3)),
            Value::from(Number::from(2)) + Value::from(Number::from(1))
        );
        assert_eq!(
            Value::from(Number::from(3)),
            &Value::from(Number::from(2)) + Value::from(Number::from(1))
        );
        assert_eq!(
            Value::from(Number::from(3)),
            &Value::from(Number::from(2)) + &Value::from(Number::from(1))
        );
        assert_eq!(
            Value::from(Number::from(3)),
            Value::from(Number::from(2)) + &Value::from(Number::from(1))
        );
    }

    #[test]
    fn smoketest_subtraction() {
        assert_eq!(
            Value::from(Number::from(1)),
            Value::from(Number::from(2)) - Value::from(Number::from(1))
        );
        assert_eq!(
            Value::from(Number::from(1)),
            &Value::from(Number::from(2)) - Value::from(Number::from(1))
        );
        assert_eq!(
            Value::from(Number::from(1)),
            &Value::from(Number::from(2)) - &Value::from(Number::from(1))
        );
        assert_eq!(
            Value::from(Number::from(1)),
            Value::from(Number::from(2)) - &Value::from(Number::from(1))
        );
    }

    #[test]
    fn smoketest_mul() {
        assert_eq!(
            Value::from(Number::from(6)),
            Value::from(Number::from(2)) * Value::from(Number::from(3))
        );
        assert_eq!(
            Value::from(Number::from(6)),
            &Value::from(Number::from(2)) * Value::from(Number::from(3))
        );
        assert_eq!(
            Value::from(Number::from(6)),
            &Value::from(Number::from(2)) * &Value::from(Number::from(3))
        );
        assert_eq!(
            Value::from(Number::from(6)),
            Value::from(Number::from(2)) * &Value::from(Number::from(3))
        );
    }

    #[test]
    fn smoketest_div() {
        assert_eq!(
            Value::from(Number::from(4)),
            Value::from(Number::from(12)) / Value::from(Number::from(3))
        );
        assert_eq!(
            Value::from(Number::from(4)),
            &Value::from(Number::from(12)) / Value::from(Number::from(3))
        );
        assert_eq!(
            Value::from(Number::from(4)),
            &Value::from(Number::from(12)) / &Value::from(Number::from(3))
        );
        assert_eq!(
            Value::from(Number::from(4)),
            Value::from(Number::from(12)) / &Value::from(Number::from(3))
        );
    }
}
