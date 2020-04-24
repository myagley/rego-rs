use std::fmt;

use ordered_float::{NotNan, OrderedFloat};
use serde::de::{self, Visitor};
use serde::{forward_to_deserialize_any, Deserialize, Deserializer, Serialize, Serializer};

use crate::Error;

#[derive(Clone, Eq, Ord, PartialEq, PartialOrd)]
pub struct Number {
    n: N,
}

#[derive(Clone, Eq, Ord, PartialEq, PartialOrd)]
enum N {
    Int(i64),
    Float(OrderedFloat<f64>),
}

impl Number {
    #[inline]
    pub fn is_i64(&self) -> bool {
        match &self.n {
            N::Int(_) => true,
            N::Float(_) => false,
        }
    }

    #[inline]
    pub fn is_f64(&self) -> bool {
        match &self.n {
            N::Float(_) => true,
            N::Int(_) => false,
        }
    }

    #[inline]
    pub fn try_into_i64(self) -> Result<i64, Error> {
        match self.n {
            N::Int(n) => Ok(n),
            N::Float(_) => Err(Error::InvalidType("i64", "f64")),
        }
    }

    #[inline]
    pub fn as_i64(&self) -> Option<i64> {
        match self.n {
            N::Int(n) => Some(n),
            N::Float(_) => None,
        }
    }

    #[inline]
    pub fn try_into_f64(self) -> Result<f64, Error> {
        match self.n {
            N::Int(n) => Ok(n as f64),
            N::Float(f) => Ok(f.into_inner()),
        }
    }

    #[inline]
    pub fn as_f64(&self) -> Option<f64> {
        match self.n {
            N::Int(n) => Some(n as f64),
            N::Float(f) => Some(f.into_inner()),
        }
    }

    #[inline]
    pub fn from_f64(f: f64) -> Option<Number> {
        if f.is_finite() {
            let n = N::Float(OrderedFloat(f));
            Some(Number { n })
        } else {
            None
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self.n {
            N::Int(i) => fmt::Display::fmt(&i, formatter),
            N::Float(f) => fmt::Display::fmt(&f, formatter),
        }
    }
}

impl fmt::Debug for Number {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        let mut debug = formatter.debug_tuple("Number");
        match self.n {
            N::Int(i) => {
                debug.field(&i);
            }
            N::Float(i) => {
                debug.field(&i);
            }
        }
        debug.finish()
    }
}

macro_rules! impl_from_int {
    ( $($ty:ty),* ) => {
        $(
            impl From<$ty> for Number {
                #[inline]
                fn from(i: $ty) -> Self {
                    let n = N::Int(i as i64);
                    Number { n }
                }
            }
        )*
    }
}

impl_from_int!(i8, u8, i16, u16, i32, u32, i64, u64, isize, usize);

macro_rules! impl_from_float {
    ( $($ty:ty),* ) => {
        $(
            impl From<$ty> for Number {
                #[inline]
                fn from(f: $ty) -> Self {
                    let n = N::Float(OrderedFloat(f.into()));
                    Number { n }
                }
            }
        )*
    }
}

impl_from_float!(f32, f64);

impl From<NotNan<f64>> for Number {
    fn from(f: NotNan<f64>) -> Self {
        let n = N::Float(OrderedFloat(f.into_inner()));
        Number { n }
    }
}

impl Serialize for Number {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self.n {
            N::Int(i) => serializer.serialize_i64(i),
            N::Float(f) => f.serialize(serializer),
        }
    }
}

impl<'de> Deserialize<'de> for Number {
    #[inline]
    fn deserialize<D>(deserializer: D) -> Result<Number, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct NumberVisitor;

        impl<'de> Visitor<'de> for NumberVisitor {
            type Value = Number;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a Rego number")
            }

            #[inline]
            fn visit_i64<E>(self, value: i64) -> Result<Number, E> {
                Ok(value.into())
            }

            #[inline]
            fn visit_f64<E>(self, value: f64) -> Result<Number, E>
            where
                E: de::Error,
            {
                Number::from_f64(value).ok_or_else(|| de::Error::custom("not a Rego number"))
            }
        }

        deserializer.deserialize_any(NumberVisitor)
    }
}

macro_rules! deserialize_any {
    (@expand [$($num_string:tt)*]) => {
        #[inline]
        fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Error>
        where
            V: Visitor<'de>,
        {
            match self.n {
                N::Int(i) => visitor.visit_i64(i),
                N::Float(f) => visitor.visit_f64(f.into_inner()),
            }
        }
    };

    (owned) => {
        deserialize_any!(@expand [n]);
    };

    (ref) => {
        deserialize_any!(@expand [n.clone()]);
    };
}

macro_rules! deserialize_number {
    ($deserialize:ident => $visit:ident) => {
        fn $deserialize<V>(self, visitor: V) -> Result<V::Value, Error>
        where
            V: Visitor<'de>,
        {
            self.deserialize_any(visitor)
        }
    }
}

impl<'de> Deserializer<'de> for Number {
    type Error = Error;

    deserialize_any!(owned);

    deserialize_number!(deserialize_i8 => visit_i8);
    deserialize_number!(deserialize_i16 => visit_i16);
    deserialize_number!(deserialize_i32 => visit_i32);
    deserialize_number!(deserialize_i64 => visit_i64);
    deserialize_number!(deserialize_u8 => visit_u8);
    deserialize_number!(deserialize_u16 => visit_u16);
    deserialize_number!(deserialize_u32 => visit_u32);
    deserialize_number!(deserialize_u64 => visit_u64);
    deserialize_number!(deserialize_f32 => visit_f32);
    deserialize_number!(deserialize_f64 => visit_f64);

    forward_to_deserialize_any! {
        bool char str string bytes byte_buf option unit unit_struct
        newtype_struct seq tuple tuple_struct map struct enum identifier
        ignored_any
    }
}

impl<'de, 'a> Deserializer<'de> for &'a Number {
    type Error = Error;

    deserialize_any!(ref);

    deserialize_number!(deserialize_i8 => visit_i8);
    deserialize_number!(deserialize_i16 => visit_i16);
    deserialize_number!(deserialize_i32 => visit_i32);
    deserialize_number!(deserialize_i64 => visit_i64);
    deserialize_number!(deserialize_u8 => visit_u8);
    deserialize_number!(deserialize_u16 => visit_u16);
    deserialize_number!(deserialize_u32 => visit_u32);
    deserialize_number!(deserialize_u64 => visit_u64);
    deserialize_number!(deserialize_f32 => visit_f32);
    deserialize_number!(deserialize_f64 => visit_f64);

    forward_to_deserialize_any! {
        bool char str string bytes byte_buf option unit unit_struct
        newtype_struct seq tuple tuple_struct map struct enum identifier
        ignored_any
    }
}
