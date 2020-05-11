use std::mem;
use std::ops::Deref;

use crate::ast::*;
use crate::value::Value;

use crate::vm::Error;

pub struct ConstEval;

impl Visitor for ConstEval {
    type Value = ();
    type Error = Error;

    fn visit_module(&mut self, _module: &mut Module) -> Result<Self::Value, Self::Error> {
        Ok(())
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Result<Self::Value, Self::Error> {
        match expr {
            Expr::BinOp(ref mut left, op, ref mut right) => {
                left.accept(self)?;
                right.accept(self)?;
                let left = &*left;
                let right = &*right;

                match (left.deref(), op, right.deref()) {
                    (Expr::Scalar(left), Opcode::Add, Expr::Scalar(right)) => {
                        let result = left + right;
                        mem::replace(expr, Expr::Scalar(result));
                    }
                    (Expr::Scalar(left), Opcode::Sub, Expr::Scalar(right)) => {
                        let result = left - right;
                        mem::replace(expr, Expr::Scalar(result));
                    }
                    (Expr::Scalar(left), Opcode::Mul, Expr::Scalar(right)) => {
                        let result = left * right;
                        mem::replace(expr, Expr::Scalar(result));
                    }
                    (Expr::Scalar(left), Opcode::Div, Expr::Scalar(right)) => {
                        let result = left / right;
                        mem::replace(expr, Expr::Scalar(result));
                    }
                    (Expr::Scalar(left), Opcode::Lt, Expr::Scalar(right)) => {
                        let result = left.lt(&right).into();
                        mem::replace(expr, Expr::Scalar(result));
                    }
                    (Expr::Scalar(left), Opcode::Lte, Expr::Scalar(right)) => {
                        let result = left.le(&right).into();
                        mem::replace(expr, Expr::Scalar(result));
                    }
                    (Expr::Scalar(left), Opcode::Gt, Expr::Scalar(right)) => {
                        let result = left.gt(&right).into();
                        mem::replace(expr, Expr::Scalar(result));
                    }
                    (Expr::Scalar(left), Opcode::Gte, Expr::Scalar(right)) => {
                        let result = left.ge(&right).into();
                        mem::replace(expr, Expr::Scalar(result));
                    }
                    (Expr::Scalar(left), Opcode::EqEq, Expr::Scalar(right)) => {
                        let result = left.eq(&right).into();
                        mem::replace(expr, Expr::Scalar(result));
                    }
                    (Expr::Scalar(left), Opcode::Ne, Expr::Scalar(right)) => {
                        let result = left.ne(&right).into();
                        mem::replace(expr, Expr::Scalar(result));
                    }
                    _ => (),
                }
            }
            Expr::Collection(collection) => match collection {
                Collection::Array(array) => {
                    let array = array
                        .into_iter()
                        .map(|e| {
                            e.accept(self)?;
                            Ok(e)
                        })
                        .collect::<Result<Vec<&mut Expr>, Self::Error>>()?;
                    if array.iter().all(|e| matches!(e, Expr::Scalar(_))) {
                        let array = array
                            .into_iter()
                            .filter_map(|e| match e {
                                Expr::Scalar(value) => Some(value.clone()),
                                _ => None,
                            })
                            .collect();
                        mem::replace(expr, Expr::Scalar(Value::Array(array)));
                    }
                }
                Collection::Set(set) => {
                    let set = set
                        .into_iter()
                        .map(|e| {
                            e.accept(self)?;
                            Ok(e)
                        })
                        .collect::<Result<Vec<&mut Expr>, Self::Error>>()?;
                    if set.iter().all(|e| matches!(e, Expr::Scalar(_))) {
                        let set = set
                            .into_iter()
                            .filter_map(|e| match e {
                                Expr::Scalar(value) => Some(value.clone()),
                                _ => None,
                            })
                            .collect();
                        mem::replace(expr, Expr::Scalar(Value::Set(set)));
                    }
                }
                Collection::Object(obj) => {
                    let obj = obj
                        .into_iter()
                        .map(|(key, val)| {
                            key.accept(self)?;
                            val.accept(self)?;
                            Ok((key, val))
                        })
                        .collect::<Result<Vec<(&mut Expr, &mut Expr)>, Self::Error>>()?;
                    if obj
                        .iter()
                        .all(|(k, v)| matches!(k, Expr::Scalar(_)) && matches!(v, Expr::Scalar(_)))
                    {
                        let obj = obj
                            .into_iter()
                            .filter_map(|(k, v)| match (k, v) {
                                (Expr::Scalar(key), Expr::Scalar(value)) => {
                                    Some((key.clone(), value.clone()))
                                }
                                _ => None,
                            })
                            .collect();
                        mem::replace(expr, Expr::Scalar(Value::Object(obj)));
                    }
                }
            },
            _ => (),
        };
        Ok(())
    }
}
