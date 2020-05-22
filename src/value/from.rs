use std::borrow::Cow;
use std::iter::FromIterator;

use super::{Map, Number, Set, Value};

macro_rules! from_integer {
    ($($ty:ident)*) => {
        $(
            impl From<$ty> for Value<'_> {
                fn from(n: $ty) -> Self {
                    Value::Number(n.into())
                }
            }
        )*
    };
}

from_integer! {
    i8 i16 i32 i64 isize
    u8 u16 u32 u64 usize
}

impl From<f32> for Value<'_> {
    fn from(f: f32) -> Self {
        From::from(f as f64)
    }
}

impl From<f64> for Value<'_> {
    fn from(f: f64) -> Self {
        Number::from_f64(f).map_or(Value::Null, Value::Number)
    }
}

impl From<bool> for Value<'_> {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

impl From<String> for Value<'_> {
    fn from(s: String) -> Self {
        Value::String(Cow::Owned(s))
    }
}

impl<'a> From<&'a str> for Value<'a> {
    fn from(s: &'a str) -> Self {
        Value::String(Cow::Borrowed(s))
    }
}

impl<'a> From<Cow<'a, str>> for Value<'a> {
    fn from(f: Cow<'a, str>) -> Self {
        Value::String(f)
    }
}

impl From<Number> for Value<'_> {
    fn from(f: Number) -> Self {
        Value::Number(f)
    }
}

impl<'a> From<Map<String, Value<'a>>> for Value<'a> {
    fn from(f: Map<String, Value<'a>>) -> Self {
        let m = f.into_iter().map(|(k, v)| (k.into(), v)).collect();
        Value::Object(m)
    }
}

impl<'a> From<Map<Value<'a>, Value<'a>>> for Value<'a> {
    fn from(f: Map<Value<'a>, Value<'a>>) -> Self {
        Value::Object(f)
    }
}

impl<'a> From<Set<Value<'a>>> for Value<'a> {
    fn from(f: Set<Value<'a>>) -> Self {
        Value::Set(f)
    }
}

impl<'a, T: Into<Value<'a>>> From<Vec<T>> for Value<'a> {
    fn from(f: Vec<T>) -> Self {
        Value::Array(f.into_iter().map(Into::into).collect())
    }
}

impl<'a, 'v, T: Clone + Into<Value<'v>>> From<&'a [T]> for Value<'v> {
    fn from(f: &'a [T]) -> Self {
        Value::Array(f.iter().cloned().map(Into::into).collect())
    }
}

impl<'a, T: Into<Value<'a>>> FromIterator<T> for Value<'a> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Value::Array(iter.into_iter().map(Into::into).collect())
    }
}

impl From<()> for Value<'_> {
    fn from((): ()) -> Self {
        Value::Null
    }
}
