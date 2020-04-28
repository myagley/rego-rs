use std::borrow::Cow;
use std::ops::{Add, Div, Mul, Sub};

use crate::value::Value;

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Operand<'a>(Cow<'a, Value<'a>>);

impl<'a> Operand<'a> {
    pub fn undefined() -> Self {
        Operand(Cow::Owned(Value::Undefined))
    }

    pub fn from_ref<'v: 'a>(value: &'v Value<'v>) -> Self {
        Operand(Cow::Borrowed(value))
    }

    pub fn from_owned(value: Value<'a>) -> Self {
        Operand(Cow::Owned(value))
    }

    pub fn into_value(self) -> Value<'a> {
        self.0.into_owned()
    }
}

impl<'a> AsRef<Value<'a>> for Operand<'a> {
    fn as_ref(&self) -> &Value<'a> {
        self.0.as_ref()
    }
}

impl<'a> AsMut<Value<'a>> for Operand<'a> {
    fn as_mut(&mut self) -> &mut Value<'a> {
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
