use std::fmt;
use std::ops::{Add, Div, Mul, Sub};

use ordered_float::{NotNan, OrderedFloat};
use serde::de::{self, Visitor};
use serde::{forward_to_deserialize_any, Deserialize, Deserializer, Serialize, Serializer};

use crate::Error;

#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
pub struct Number {
    n: N,
}

#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
enum N {
    PosInt(u64),
    NegInt(i64),
    Float(OrderedFloat<f64>),
}

impl Number {
    #[inline]
    pub fn is_i64(&self) -> bool {
        match self.n {
            N::PosInt(v) => v <= i64::max_value() as u64,
            N::NegInt(_) => true,
            N::Float(_) => false,
        }
    }

    #[inline]
    pub fn is_u64(&self) -> bool {
        match self.n {
            N::PosInt(_) => true,
            N::NegInt(_) | N::Float(_) => false,
        }
    }

    #[inline]
    pub fn is_f64(&self) -> bool {
        match &self.n {
            N::Float(_) => true,
            N::PosInt(_) | N::NegInt(_) => false,
        }
    }

    #[inline]
    pub fn try_into_i64(self) -> Result<i64, Error> {
        match self.n {
            N::PosInt(n) => {
                if n <= i64::max_value() as u64 {
                    Ok(n as i64)
                } else {
                    Err(Error::InvalidType("i64", "u64"))
                }
            }
            N::NegInt(n) => Ok(n),
            N::Float(_) => Err(Error::InvalidType("i64", "f64")),
        }
    }

    #[inline]
    pub fn as_i64(&self) -> Option<i64> {
        match self.n {
            N::PosInt(n) => {
                if n <= i64::max_value() as u64 {
                    Some(n as i64)
                } else {
                    None
                }
            }
            N::NegInt(n) => Some(n),
            N::Float(_) => None,
        }
    }

    #[inline]
    pub fn try_into_f64(self) -> Result<f64, Error> {
        match self.n {
            N::PosInt(n) => Ok(n as f64),
            N::NegInt(n) => Ok(n as f64),
            N::Float(f) => Ok(f.into_inner()),
        }
    }

    #[inline]
    pub fn as_f64(&self) -> Option<f64> {
        match self.n {
            N::PosInt(n) => Some(n as f64),
            N::NegInt(n) => Some(n as f64),
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
            N::PosInt(i) => fmt::Display::fmt(&i, formatter),
            N::NegInt(i) => fmt::Display::fmt(&i, formatter),
            N::Float(f) => fmt::Display::fmt(&f, formatter),
        }
    }
}

impl fmt::Debug for Number {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        let mut debug = formatter.debug_tuple("Number");
        match self.n {
            N::PosInt(i) => {
                debug.field(&i);
            }
            N::NegInt(i) => {
                debug.field(&i);
            }
            N::Float(i) => {
                debug.field(&i);
            }
        }
        debug.finish()
    }
}

macro_rules! impl_from_unsigned {
    ( $($ty:ty),* ) => {
        $(
            impl From<$ty> for Number {
                #[inline]
                fn from(i: $ty) -> Self {
                    let n = N::PosInt(i as u64);
                    Number { n }
                }
            }
        )*
    }
}

macro_rules! impl_from_signed {
    ( $($ty:ty),* ) => {
        $(
            impl From<$ty> for Number {
                #[inline]
                fn from(i: $ty) -> Self {
                    let n = if i < 0 {
                        N::NegInt(i as i64)
                    } else {
                        N::PosInt(i as u64)
                    };
                    Number { n }
                }
            }
        )*
    }
}

impl_from_unsigned!(u8, u16, u32, u64, usize);
impl_from_signed!(i8, i16, i32, i64, isize);

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
            N::PosInt(i) => serializer.serialize_u64(i),
            N::NegInt(i) => serializer.serialize_i64(i),
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
            fn visit_u64<E>(self, value: u64) -> Result<Number, E> {
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
                N::PosInt(i) => visitor.visit_u64(i),
                N::NegInt(i) => visitor.visit_i64(i),
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

macro_rules! forward_ref_binop {
    (impl $imp:ident, $method:ident for $t:ty, $u:ty) => {
        impl<'a> $imp<$u> for &'a $t {
            type Output = <$t as $imp<$u>>::Output;

            #[inline]
            fn $method(self, other: $u) -> <$t as $imp<$u>>::Output {
                $imp::$method(*self, other)
            }
        }

        impl $imp<&$u> for $t {
            type Output = <$t as $imp<$u>>::Output;

            #[inline]
            fn $method(self, other: &$u) -> <$t as $imp<$u>>::Output {
                $imp::$method(self, *other)
            }
        }

        impl $imp<&$u> for &$t {
            type Output = <$t as $imp<$u>>::Output;

            #[inline]
            fn $method(self, other: &$u) -> <$t as $imp<$u>>::Output {
                $imp::$method(*self, *other)
            }
        }
    };
}

macro_rules! impl_binop {
    (impl $imp:ident, $method:ident for $t:ty) => {
        impl $imp for $t {
            type Output = $t;

            fn $method(self, other: Self) -> Self {
                match (self.n, other.n) {
                    (N::PosInt(l), N::PosInt(r)) => Number::from($imp::$method(l, r)),
                    (N::PosInt(l), N::NegInt(r)) => {
                        if r < 0 {
                            Number::from($imp::$method((l as i64), r))
                        } else {
                            Number::from($imp::$method(l, r as u64))
                        }
                    }
                    (N::PosInt(l), N::Float(r)) => {
                        Number::from($imp::$method(l as f64, r.into_inner()))
                    }

                    (N::Float(l), N::Float(r)) => {
                        Number::from($imp::$method(l.into_inner(), r.into_inner()))
                    }
                    (N::Float(l), N::PosInt(r)) => {
                        Number::from($imp::$method(l.into_inner(), r as f64))
                    }
                    (N::Float(l), N::NegInt(r)) => {
                        Number::from($imp::$method(l.into_inner(), r as f64))
                    }

                    (N::NegInt(l), N::NegInt(r)) => Number::from($imp::$method(l, r)),
                    (N::NegInt(l), N::PosInt(r)) => {
                        if l < 0 {
                            Number::from($imp::$method(l, (r as i64)))
                        } else {
                            Number::from($imp::$method(l as u64, r))
                        }
                    }
                    (N::NegInt(l), N::Float(r)) => {
                        Number::from($imp::$method(l as f64, r.into_inner()))
                    }
                }
            }
        }

        forward_ref_binop! { impl $imp, $method for $t, $t }
    };
}

impl_binop!(impl Add, add for Number);
impl_binop!(impl Sub, sub for Number);
impl_binop!(impl Mul, mul for Number);
impl_binop!(impl Div, div for Number);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoketest_addition() {
        assert_eq!(Number::from(3), Number::from(2) + Number::from(1));
    }

    #[test]
    fn smoketest_subtraction() {
        assert_eq!(Number::from(1), Number::from(2) - Number::from(1));
    }

    #[test]
    fn smoketest_mul() {
        assert_eq!(Number::from(6), Number::from(2) * Number::from(3));
    }

    #[test]
    fn smoketest_div() {
        assert_eq!(Number::from(6), Number::from(12) / Number::from(2));
    }
}
