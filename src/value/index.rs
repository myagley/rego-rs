use std::collections::BTreeMap;
use std::ops;

use super::{Type, Value};

pub trait Index: private::Sealed {
    #[doc(hidden)]
    fn index_into<'a, 'v: 'a>(&self, v: &'a Value<'v>) -> Option<&'a Value<'v>>;

    #[doc(hidden)]
    fn index_into_mut<'a, 'v: 'a>(&self, v: &'a mut Value<'v>) -> Option<&'a mut Value<'v>>;

    #[doc(hidden)]
    fn index_or_insert<'a, 'v: 'a>(&self, v: &'a mut Value<'v>) -> &'a mut Value<'v>;
}

impl Index for usize {
    fn index_into<'a, 'v: 'a>(&self, v: &'a Value<'v>) -> Option<&'a Value<'v>> {
        match v {
            Value::Array(ref vec) => vec.get(*self),
            _ => None,
        }
    }

    fn index_into_mut<'a, 'v: 'a>(&self, v: &'a mut Value<'v>) -> Option<&'a mut Value<'v>> {
        match v {
            Value::Array(ref mut vec) => vec.get_mut(*self),
            _ => None,
        }
    }

    fn index_or_insert<'a, 'v: 'a>(&self, v: &'a mut Value<'v>) -> &'a mut Value<'v> {
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

impl Index for str {
    fn index_into<'a, 'v: 'a>(&self, v: &'a Value<'v>) -> Option<&'a Value<'v>> {
        match *v {
            Value::Object(ref map) => map.get(self),
            _ => None,
        }
    }

    fn index_into_mut<'a, 'v: 'a>(&self, v: &'a mut Value<'v>) -> Option<&'a mut Value<'v>> {
        match v {
            Value::Object(ref mut map) => map.get_mut(self),
            _ => None,
        }
    }

    fn index_or_insert<'a, 'v: 'a>(&self, v: &'a mut Value<'v>) -> &'a mut Value<'v> {
        if let Value::Null = *v {
            *v = Value::Object(BTreeMap::new());
        }
        match v {
            Value::Object(ref mut map) => map.entry(self.to_owned()).or_insert(Value::Null),
            _ => panic!("cannot access key {:?} in value {}", self, Type(v)),
        }
    }
}

impl Index for String {
    fn index_into<'a, 'v: 'a>(&self, v: &'a Value<'v>) -> Option<&'a Value<'v>> {
        self[..].index_into(v)
    }

    fn index_into_mut<'a, 'v: 'a>(&self, v: &'a mut Value<'v>) -> Option<&'a mut Value<'v>> {
        self[..].index_into_mut(v)
    }

    fn index_or_insert<'a, 'v: 'a>(&self, v: &'a mut Value<'v>) -> &'a mut Value<'v> {
        self[..].index_or_insert(v)
    }
}

impl<'b, T: ?Sized> Index for &'b T
where
    T: Index,
{
    fn index_into<'a, 'v: 'a>(&self, v: &'a Value<'v>) -> Option<&'a Value<'v>> {
        (**self).index_into(v)
    }
    fn index_into_mut<'a, 'v: 'a>(&self, v: &'a mut Value<'v>) -> Option<&'a mut Value<'v>> {
        (**self).index_into_mut(v)
    }
    fn index_or_insert<'a, 'v: 'a>(&self, v: &'a mut Value<'v>) -> &'a mut Value<'v> {
        (**self).index_or_insert(v)
    }
}

mod private {
    pub trait Sealed {}
    impl Sealed for usize {}
    impl Sealed for str {}
    impl Sealed for String {}
    impl<'a, T: ?Sized> Sealed for &'a T where T: Sealed {}
}

impl<'v, I> ops::Index<I> for Value<'v>
where
    I: Index,
{
    type Output = Value<'v>;

    fn index(&self, index: I) -> &Value<'v> {
        static NULL: Value = Value::Null;
        index.index_into(self).unwrap_or(&NULL)
    }
}

impl<'v, I> ops::IndexMut<I> for Value<'v>
where
    I: Index,
{
    fn index_mut(&mut self, index: I) -> &mut Value<'v> {
        index.index_or_insert(self)
    }
}
