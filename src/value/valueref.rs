use std::borrow::Cow;
use std::fmt;

use crate::value::{Map, Number, Value};

pub trait Index {
    fn index(&self, v: &Value<'_>) -> Option<Value<'_>>;
}

pub trait ToValue {
    fn to_value(&self) -> Value<'_>;
}

pub trait ValueRef: Index + ToValue + fmt::Debug {}

impl<T> ValueRef for T where T: Index + ToValue + fmt::Debug {}

impl ToValue for Value<'_> {
    fn to_value(&self) -> Value<'_> {
        match self {
            Value::Undefined => Value::Undefined,
            Value::Null => Value::Null,
            Value::Bool(b) => Value::Bool(*b),
            Value::Number(n) => Value::Number(*n),
            Value::String(s) => Value::String(Cow::Borrowed(s.as_ref())),
            // todo Cow the array and map?
            Value::Object(m) => Value::Object(m.clone()),
            Value::Array(a) => Value::Array(a.clone()),
            Value::Set(s) => Value::Set(s.clone()),
            Value::Ref(r) => r.to_value(),
        }
    }
}

impl Index for Value<'_> {
    fn index(&self, field: &Value<'_>) -> Option<Value<'_>> {
        match self {
            Value::Ref(r) => r.index(field),
            Value::Object(m) => m.index(field),
            Value::Array(a) => a.index(field),
            _ => None,
        }
    }
}

impl<'v, T> Index for Vec<T>
where
    T: ToValue + std::fmt::Debug,
{
    fn index(&self, field: &Value<'_>) -> Option<Value<'_>> {
        if let Some(n) = field.as_u64() {
            self.get(n as usize).map(ToValue::to_value)
        } else {
            None
        }
    }
}

impl<'v, T> ToValue for Vec<T>
where
    T: ToValue + std::fmt::Debug,
{
    fn to_value(&self) -> Value<'_> {
        let array = self
            .iter()
            .map(|v| v.to_value())
            .collect::<Vec<Value<'_>>>();
        Value::Array(array)
    }
}

impl<'v, T> Index for Map<String, T>
where
    T: ToValue + std::fmt::Debug,
{
    fn index(&self, field: &Value<'_>) -> Option<Value<'_>> {
        if let Value::String(s) = field {
            self.get(s.as_ref()).map(ToValue::to_value)
        } else {
            None
        }
    }
}

impl<'v, T> ToValue for Map<String, T>
where
    T: ToValue + std::fmt::Debug,
{
    fn to_value(&self) -> Value<'_> {
        let map = self
            .iter()
            .map(|(k, v)| (Value::String(Cow::Borrowed(k)), v.to_value()))
            .collect::<Map<Value<'_>, Value<'_>>>();
        Value::Object(map)
    }
}

impl Index for Map<Value<'_>, Value<'_>> {
    fn index(&self, field: &Value<'_>) -> Option<Value<'_>> {
        if let Some(value) = self.get(field) {
            Some(value.clone())
        } else {
            None
        }
    }
}

impl ToValue for Map<Value<'_>, Value<'_>> {
    fn to_value(&self) -> Value<'_> {
        Value::Object(self.clone())
    }
}

impl Index for () {
    fn index(&self, _field: &Value<'_>) -> Option<Value<'_>> {
        None
    }
}

impl ToValue for () {
    fn to_value(&self) -> Value<'_> {
        Value::Null
    }
}

macro_rules! impl_valueref_num {
    (impl $ty:ty) => {
        impl Index for $ty {
            fn index(&self, _field: &Value<'_>) -> Option<Value<'_>> {
                None
            }
        }

        impl ToValue for $ty {
            fn to_value(&self) -> Value<'_> {
                Value::Number(Number::from(*self))
            }
        }
    };
}

impl_valueref_num!(impl u8);
impl_valueref_num!(impl u16);
impl_valueref_num!(impl u32);
impl_valueref_num!(impl u64);
impl_valueref_num!(impl i8);
impl_valueref_num!(impl i16);
impl_valueref_num!(impl i32);
impl_valueref_num!(impl i64);
impl_valueref_num!(impl f32);
impl_valueref_num!(impl f64);
impl_valueref_num!(impl usize);
impl_valueref_num!(impl isize);

impl Index for String {
    fn index(&self, _field: &Value<'_>) -> Option<Value<'_>> {
        None
    }
}

impl ToValue for String {
    fn to_value(&self) -> Value<'_> {
        Value::String(Cow::Borrowed(self.as_str()))
    }
}

impl Index for str {
    fn index(&self, _field: &Value<'_>) -> Option<Value<'_>> {
        None
    }
}

impl ToValue for str {
    fn to_value(&self) -> Value<'_> {
        Value::String(Cow::Borrowed(self))
    }
}

impl<T> Index for Option<T>
where
    T: Index,
{
    fn index(&self, field: &Value<'_>) -> Option<Value<'_>> {
        if let Some(v) = self {
            v.index(field)
        } else {
            None
        }
    }
}

impl<T> ToValue for Option<T>
where
    T: ToValue,
{
    fn to_value(&self) -> Value<'_> {
        if let Some(v) = self {
            ToValue::to_value(v)
        } else {
            Value::Null
        }
    }
}
