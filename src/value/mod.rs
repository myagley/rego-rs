use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::ops::{Add, Div, Mul, Sub};
use std::hash::{Hash, Hasher};

mod de;
mod from;
mod number;
mod ser;
mod valueref;

pub use self::number::Number;
pub use self::valueref::{Index, ToValue, ValueRef};

pub type Map<K, V> = BTreeMap<K, V>;
pub type Set<V> = BTreeSet<V>;

#[derive(Clone, Debug)]
pub struct InvalidType(&'static str, &'static str);

impl fmt::Display for InvalidType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Invalid type when converting: expected {}, got {}",
            self.0, self.1
        )
    }
}

impl std::error::Error for InvalidType {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

#[derive(Clone)]
pub enum Value<'v> {
    Undefined,
    Null,
    Bool(bool),
    Number(Number),
    String(Cow<'v, str>),
    Array(Vec<Value<'v>>),
    Object(Map<Value<'v>, Value<'v>>),
    Set(Set<Value<'v>>),
    Ref(&'v dyn ValueRef),
}

impl PartialEq for Value<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Undefined, Value::Undefined) => true,
            (Value::Null, Value::Null) => true,
            (Value::Bool(l), Value::Bool(r)) => l == r,
            (Value::Number(l), Value::Number(r)) => l == r,
            (Value::String(l), Value::String(r)) => l == r,
            (Value::Array(l), Value::Array(r)) => l == r,
            (Value::Object(l), Value::Object(r)) => l == r,
            (Value::Ref(l), Value::Ref(r)) => l.to_value().eq(&r.to_value()),
            (Value::Ref(l), r) => l.to_value().eq(&r),
            (l, Value::Ref(r)) => l.eq(&r.to_value()),
            _ => false,
        }
    }
}

impl Eq for Value<'_> {}

impl Ord for Value<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Value::Undefined, Value::Undefined) => Ordering::Equal,
            (Value::Null, Value::Null) => Ordering::Equal,
            (Value::Bool(l), Value::Bool(r)) => l.cmp(r),
            (Value::Number(l), Value::Number(r)) => l.cmp(r),
            (Value::String(l), Value::String(r)) => l.cmp(r),
            (Value::Array(l), Value::Array(r)) => l.cmp(r),
            (Value::Object(l), Value::Object(r)) => l.cmp(r),
            (Value::Ref(l), Value::Ref(r)) => l.to_value().cmp(&r.to_value()),
            (Value::Ref(l), r) => l.to_value().cmp(&r),
            (l, Value::Ref(r)) => l.cmp(&r.to_value()),
            (Value::Undefined, _) => Ordering::Less,
            (_, Value::Undefined) => Ordering::Greater,
            (Value::Null, _) => Ordering::Less,
            (_, Value::Null) => Ordering::Greater,
            (Value::Number(_), _) => Ordering::Less,
            (_, Value::Number(_)) => Ordering::Greater,
            (Value::String(_), _) => Ordering::Less,
            (_, Value::String(_)) => Ordering::Greater,
            (Value::Array(_), _) => Ordering::Less,
            (_, Value::Array(_)) => Ordering::Greater,
            (Value::Object(_), _) => Ordering::Less,
            (_, Value::Object(_)) => Ordering::Greater,
            (Value::Set(_), _) => Ordering::Less,
            (_, Value::Set(_)) => Ordering::Greater,
        }
    }
}

impl PartialOrd for Value<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Debug for Value<'_> {
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
            Value::Ref(ref v) => formatter.debug_tuple("Ref").field(v).finish(),
        }
    }
}

impl Hash for Value<'static> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Undefined => ().hash(state),
            Value::Null => ().hash(state),
            Value::Bool(v) => v.hash(state),
            Value::Number(ref v) => v.hash(state),
            Value::String(ref v) => v.hash(state),
            Value::Array(ref v) => v.hash(state),
            Value::Object(ref v) => v.hash(state),
            Value::Set(ref v) => v.hash(state),
            Value::Ref(ref _v) => ().hash(state),
        }
    }
}

impl fmt::Display for Value<'_> {
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
            Value::Ref(ref v) => write!(f, "{:?}", v),
        }
    }
}

impl Default for Value<'_> {
    fn default() -> Value<'static> {
        Value::Undefined
    }
}

impl<'v> Value<'v> {
    pub fn get(&self, index: &Value<'v>) -> Option<Value<'_>> {
        match self {
            Value::Array(ref vec) => vec.index(index),
            Value::Object(ref map) => map.index(index),
            Value::Ref(ref r) => r.index(index),
            _ => None,
        }
    }

    pub fn try_into_set(self) -> Result<Set<Value<'v>>, InvalidType> {
        match self {
            Value::Set(v) => Ok(v),
            v => Err(InvalidType("set", Type(&v).ty())),
        }
    }

    pub fn as_set(&self) -> Option<&Set<Value>> {
        match *self {
            Value::Set(ref set) => Some(set),
            _ => None,
        }
    }

    pub fn as_set_mut(&mut self) -> Option<&mut Set<Value<'v>>> {
        match *self {
            Value::Set(ref mut set) => Some(set),
            _ => None,
        }
    }

    pub fn is_set(&self) -> bool {
        self.as_set().is_some()
    }

    pub fn try_into_object(self) -> Result<Map<Value<'v>, Value<'v>>, InvalidType> {
        match self {
            Value::Object(map) => Ok(map),
            v => Err(InvalidType("object", Type(&v).ty())),
        }
    }

    pub fn as_object(&self) -> Option<&Map<Value<'v>, Value<'v>>> {
        match *self {
            Value::Object(ref map) => Some(map),
            _ => None,
        }
    }

    pub fn as_object_mut(&mut self) -> Option<&mut Map<Value<'v>, Value<'v>>> {
        match *self {
            Value::Object(ref mut map) => Some(map),
            _ => None,
        }
    }

    pub fn is_object(&self) -> bool {
        self.as_object().is_some()
    }

    pub fn try_into_array(self) -> Result<Vec<Value<'v>>, InvalidType> {
        match self {
            Value::Array(array) => Ok(array),
            v => Err(InvalidType("array", Type(&v).ty())),
        }
    }

    pub fn as_array(&self) -> Option<&Vec<Value>> {
        match *self {
            Value::Array(ref array) => Some(array),
            _ => None,
        }
    }

    pub fn as_array_mut(&mut self) -> Option<&mut Vec<Value<'v>>> {
        match *self {
            Value::Array(ref mut array) => Some(array),
            _ => None,
        }
    }

    pub fn is_array(&self) -> bool {
        self.as_array().is_some()
    }

    pub fn try_into_string(self) -> Result<String, InvalidType> {
        match self {
            Value::String(string) => Ok(string.into_owned()),
            v => Err(InvalidType("string", Type(&v).ty())),
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

    pub fn try_into_i64(self) -> Result<i64, InvalidType> {
        match self {
            Value::Number(n) => n.try_into_i64(),
            v => Err(InvalidType("i64", Type(&v).ty())),
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

    pub fn try_into_u64(self) -> Result<u64, InvalidType> {
        match self {
            Value::Number(n) => n.try_into_u64(),
            v => Err(InvalidType("u64", Type(&v).ty())),
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

    pub fn try_into_f64(self) -> Result<f64, InvalidType> {
        match self {
            Value::Number(n) => n.try_into_f64(),
            v => Err(InvalidType("f64", Type(&v).ty())),
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

    pub fn try_into_bool(self) -> Result<bool, InvalidType> {
        match self {
            Value::Bool(b) => Ok(b),
            v => Err(InvalidType("bool", Type(&v).ty())),
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

    pub fn is_true(&self) -> bool {
        self.as_bool().unwrap_or(false)
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

struct Type<'a>(&'a Value<'a>);

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
            Value::Ref(_) => "ref",
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
            Value::Ref(_) => formatter.write_str("ref"),
        }
    }
}

macro_rules! impl_binop {
    (impl $imp:ident, $method:ident) => {
        impl $imp for Value<'_> {
            type Output = Value<'static>;

            fn $method(self, other: Self) -> Self::Output {
                match (self, other) {
                    (Value::Number(l), Value::Number(r)) => Value::Number($imp::$method(l, r)),
                    _ => Value::Undefined,
                }
            }
        }

        impl<'a> $imp<Value<'_>> for &'a Value<'_> {
            type Output = Value<'static>;

            fn $method(self, other: Value) -> Self::Output {
                match (self, other) {
                    (Value::Number(ref l), Value::Number(r)) => Value::Number($imp::$method(*l, r)),
                    _ => Value::Undefined,
                }
            }
        }

        impl<'v, 'a: 'v> $imp<&'a Value<'v>> for Value<'v> {
            type Output = Value<'static>;

            fn $method(self, other: &'a Self) -> Self::Output {
                match (self, other) {
                    (Value::Number(l), Value::Number(ref r)) => Value::Number($imp::$method(l, *r)),
                    _ => Value::Undefined,
                }
            }
        }

        impl<'v, 'a: 'v> $imp<&'a Value<'v>> for &'v Value<'v> {
            type Output = Value<'static>;

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

pub fn to_value<T>(value: T) -> Result<Value<'static>, crate::Error<'static>>
where
    T: serde::Serialize,
{
    value.serialize(ser::Serializer)
}

pub fn from_value<'a, T>(value: Value<'a>) -> Result<T, crate::Error<'static>>
where
    T: serde::de::DeserializeOwned,
{
    T::deserialize(value)
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::collections::HashMap;

    use proptest::collection;
    use proptest::prelude::*;
    use serde::{Deserialize, Serialize};

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

    macro_rules! test_serde_primitive {
        ($method:ident, $ty:ty) => {
            proptest! {
                #[test]
                fn $method(b in any::<$ty>()) {
                    let value = to_value(b).unwrap();
                    let result = from_value::<$ty>(value).unwrap();
                    assert_eq!(b, result)
                }
            }
        };
    }

    test_serde_primitive!(test_serde_bool, bool);
    test_serde_primitive!(test_serde_i8, i8);
    test_serde_primitive!(test_serde_u8, u8);
    test_serde_primitive!(test_serde_i16, i16);
    test_serde_primitive!(test_serde_u16, u16);
    test_serde_primitive!(test_serde_i32, i32);
    test_serde_primitive!(test_serde_u32, u32);
    test_serde_primitive!(test_serde_i64, i64);
    test_serde_primitive!(test_serde_u64, u64);

    proptest! {
        #[test]
        fn test_serde_string(s in "\\PC*") {
            let value = to_value(s.clone()).unwrap();
            let result = from_value::<String>(value).unwrap();
            assert_eq!(s, result)
        }
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    struct UnitStruct;

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    struct NewTypeStruct(i64);

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    struct TupleStruct(i64, String);

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    enum TestEnum {
        Unit,
        NewType(i64),
        Tuple(i64, String),
        Struct { age: i64, msg: String },
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    struct Person {
        name: String,
        age: u8,
        properties: HashMap<String, String>,
    }

    prop_compose! {
        fn arb_person()(
            name in any::<String>(),
            age in any::<u8>(),
            properties in collection::hash_map(any::<String>(), any::<String>(), 0..10)
        ) -> Person {
            Person {
                name,
                age,
                properties,
            }
        }
    }

    proptest! {
        #[test]
        fn test_serde_struct(person in arb_person()) {
            let value = to_value(person.clone()).unwrap();
            let result = from_value(value).unwrap();
            assert_eq!(person, result);
        }
    }

    #[test]
    fn test_serde_unit_struct() {
        let item = UnitStruct;
        let value = to_value(item.clone()).unwrap();
        let result = from_value(value).unwrap();
        assert_eq!(item, result);
    }

    proptest! {
        #[test]
        fn test_serde_new_type_struct(age in any::<i64>()) {
            let item = NewTypeStruct(age);
            let value = to_value(item.clone()).unwrap();
            let result = from_value(value).unwrap();
            assert_eq!(item, result);
        }
    }

    proptest! {
        #[test]
        fn test_serde_tuple_struct(age in any::<i64>(), name in any::<String>()) {
            let item = TupleStruct(age, name);
            let value = to_value(item.clone()).unwrap();
            let result = from_value(value).unwrap();
            assert_eq!(item, result);
        }
    }

    prop_compose! {
        fn arb_enum_tuple()(age in any::<i64>(), name in any::<String>()) -> TestEnum {
            TestEnum::Tuple(age, name)
        }
    }

    prop_compose! {
        fn arb_enum_struct()(age in any::<i64>(), msg in any::<String>()) -> TestEnum {
            TestEnum::Struct { age, msg }
        }
    }

    fn arb_testenum() -> impl Strategy<Value = TestEnum> {
        prop_oneof![
            Just(TestEnum::Unit),
            any::<i64>().prop_map(TestEnum::NewType),
            arb_enum_tuple(),
            arb_enum_struct(),
        ]
    }

    proptest! {
        #[test]
        fn test_serde_enum(item in arb_testenum()) {
            let value = to_value(item.clone()).unwrap();
            let result = from_value(value).unwrap();
            assert_eq!(item, result);
        }
    }
}
