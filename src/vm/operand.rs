use std::borrow::{Borrow, Cow};
use std::ops::{Add, Deref, Div, Mul, Sub};

use crate::value::Value;

#[derive(Debug, PartialEq)]
pub struct Operand<'a>(Option<Cow<'a, Value<'a>>>);

impl<'a> Operand<'a> {
    pub fn undefined() -> Self {
        Operand(None)
    }

    pub fn from_ref<'v: 'a>(value: &'v Value<'v>) -> Self {
        Operand(Some(Cow::Borrowed(value)))
    }

    pub fn from_owned(value: Value<'a>) -> Self {
        Operand(Some(Cow::Owned(value)))
    }

    pub fn into_value(self) -> Option<Value<'a>> {
        self.0.map(|v| v.into_owned())
    }
}

macro_rules! impl_binop {
    (impl $imp:ident, $method:ident) => {
        impl<'v, 'a: 'v> $imp for Operand<'v> {
            type Output = Operand<'v>;

            fn $method(self, other: Self) -> Self::Output {
                match (self.0, other.0) {
                    (Some(left), Some(right)) => {
                        Operand($imp::$method(left.as_ref(), right.as_ref()).map(Cow::Owned))
                    }
                    _ => Operand::undefined(),
                }
            }
        }
    };
}

impl_binop!(impl Add, add);
impl_binop!(impl Sub, sub);
impl_binop!(impl Mul, mul);
impl_binop!(impl Div, div);
