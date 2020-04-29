use std::ops;

use super::{Type, Value};

pub trait Index: private::Sealed {
    #[doc(hidden)]
    fn index_into<'v>(&self, v: &'v Value) -> Option<&'v Value>;

    #[doc(hidden)]
    fn index_into_mut<'v>(&self, v: &'v mut Value) -> Option<&'v mut Value>;

    #[doc(hidden)]
    fn index_or_insert<'v>(&self, v: &'v mut Value) -> &'v mut Value;
}

impl Index for usize {
    fn index_into<'v>(&self, v: &'v Value) -> Option<&'v Value> {
        match v {
            Value::Array(ref vec) => vec.get(*self),
            _ => None,
        }
    }

    fn index_into_mut<'v>(&self, v: &'v mut Value) -> Option<&'v mut Value> {
        match v {
            Value::Array(ref mut vec) => vec.get_mut(*self),
            _ => None,
        }
    }

    fn index_or_insert<'v>(&self, v: &'v mut Value) -> &'v mut Value {
        match v {
            Value::Array(ref mut vec) => {
                let len = vec.len();
                vec.get_mut(*self).unwrap_or_else(|| {
                    panic!("cannot access index {} of array of length {}", self, len)
                })
            }
            _ => panic!("cannot access index {} of value {}", self, Type(v)),
        }
    }
}

impl Index for Value {
    fn index_into<'v>(&self, v: &'v Value) -> Option<&'v Value> {
        match v {
            Value::Array(ref vec) => {
                self.as_u64().and_then(|u| vec.get(u as usize))
            }
            Value::Object(ref map) => {
                map.get(self)
            }
            _ => None,
        }
    }

    fn index_into_mut<'v>(&self, v: &'v mut Value) -> Option<&'v mut Value> {
        match v {
            Value::Array(ref mut vec) => {
                self.as_u64().and_then(move |u| vec.get_mut(u as usize))
            }
            Value::Object(ref mut map) => {
                map.get_mut(self)
            }
            _ => None,
        }
    }

    fn index_or_insert<'v>(&self, v: &'v mut Value) -> &'v mut Value {
        match v {
            Value::Array(ref mut vec) => {
                let len = vec.len();
                self.as_u64().and_then(move |u| vec.get_mut(u as usize)).unwrap_or_else(move || {
                    panic!("cannot access index {} of array of length {}", self, len)
                })
            }
            Value::Object(ref mut map) => {
                map.get_mut(self).unwrap_or_else(|| {
                    panic!("cannot access key {} of object", self)
                })
            }
            _ => panic!("cannot access index {} of value {}", self, Type(v)),
        }
    }
}

// impl Index for str {
//     fn index_into<'v>(&self, v: &'v Value) -> Option<&'v Value> {
//         match *v {
//             Value::Object(ref map) => map.get(self),
//             _ => None,
//         }
//     }
//
//     fn index_into_mut<'v>(&self, v: &'v mut Value) -> Option<&'v mut Value> {
//         match v {
//             Value::Object(ref mut map) => map.get_mut(self),
//             _ => None,
//         }
//     }
//
//     fn index_or_insert<'v>(&self, v: &'v mut Value) -> &'v mut Value {
//         if let Value::Null = *v {
//             *v = Value::Object(BTreeMap::new());
//         }
//         match v {
//             Value::Object(ref mut map) => map.entry(self.to_owned()).or_insert(Value::Null),
//             _ => panic!("cannot access key {:?} in value {}", self, Type(v)),
//         }
//     }
// }
//
// impl Index for String {
//     fn index_into<'v>(&self, v: &'v Value) -> Option<&'v Value> {
//         self[..].index_into(v)
//     }
//
//     fn index_into_mut<'v>(&self, v: &'v mut Value) -> Option<&'v mut Value> {
//         self[..].index_into_mut(v)
//     }
//
//     fn index_or_insert<'v>(&self, v: &'v mut Value) -> &'v mut Value {
//         self[..].index_or_insert(v)
//     }
// }

impl<'a, T: ?Sized> Index for &'a T
where
    T: Index,
{
    fn index_into<'v>(&self, v: &'v Value) -> Option<&'v Value> {
        (**self).index_into(v)
    }
    fn index_into_mut<'v>(&self, v: &'v mut Value) -> Option<&'v mut Value> {
        (**self).index_into_mut(v)
    }
    fn index_or_insert<'v>(&self, v: &'v mut Value) -> &'v mut Value {
        (**self).index_or_insert(v)
    }
}

mod private {
    use super::*;

    pub trait Sealed {}
    impl Sealed for usize {}
    impl Sealed for Value {}
    impl Sealed for str {}
    impl Sealed for String {}
    impl<'a, T: ?Sized> Sealed for &'a T where T: Sealed {}
}

impl<I> ops::Index<I> for Value
where
    I: Index,
{
    type Output = Value;

    fn index(&self, index: I) -> &Value {
        static UNDEFINED: Value = Value::Undefined;
        index.index_into(self).unwrap_or(&UNDEFINED)
    }
}

impl<I> ops::IndexMut<I> for Value
where
    I: Index,
{
    fn index_mut(&mut self, index: I) -> &mut Value {
        index.index_or_insert(self)
    }
}
