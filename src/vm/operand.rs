use std::borrow::Cow;
use std::ops::{Add, Div, Mul, Sub};

use crate::value::Value;

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Operand<'a>(Cow<'a, Value>);

impl<'a> Operand<'a> {
    pub fn from_ref(value: &'a Value) -> Self {
        Operand(Cow::Borrowed(value))
    }

    pub fn from_owned(value: Value) -> Self {
        Operand(Cow::Owned(value))
    }

    pub fn into_value(self) -> Value {
        self.0.into_owned()
    }
}

impl<'a> AsRef<Value> for Operand<'a> {
    fn as_ref(&self) -> &Value {
        self.0.as_ref()
    }
}

impl<'a> AsMut<Value> for Operand<'a> {
    fn as_mut(&mut self) -> &mut Value {
        self.0.to_mut()
    }
}

macro_rules! impl_binop {
    (impl $imp:ident, $method:ident) => {
        impl<'v, 'a: 'v> $imp for Operand<'v> {
            type Output = Operand<'v>;

            fn $method(self, other: Self) -> Self::Output {
                Operand::from_owned($imp::$method(self.0.as_ref(), other.0.as_ref()))
            }
        }
    };
}

impl_binop!(impl Add, add);
impl_binop!(impl Sub, sub);
impl_binop!(impl Mul, mul);
impl_binop!(impl Div, div);
