use crate::ast::*;
use crate::value::Value;

use crate::vm::Error;

pub struct ConstEval;

impl Visitor for &ConstEval {
    type Value = Expr;
    type Error = Error;

    fn visit_expr(self, expr: Expr) -> Result<Self::Value, Self::Error> {
        let expr = match expr {
            Expr::Scalar(value) => Expr::Scalar(value),
            Expr::Collection(collection) => collection.accept(&*self)?,
            Expr::BinOp(left, op, right) => {
                let left = left.accept(&*self)?;
                let right = right.accept(&*self)?;
                match (left, op, right) {
                    (Expr::Scalar(left), Opcode::Add, Expr::Scalar(right)) => {
                        Expr::Scalar(left + right)
                    }
                    (Expr::Scalar(left), Opcode::Sub, Expr::Scalar(right)) => {
                        Expr::Scalar(left - right)
                    }
                    (Expr::Scalar(left), Opcode::Mul, Expr::Scalar(right)) => {
                        Expr::Scalar(left * right)
                    }
                    (Expr::Scalar(left), Opcode::Div, Expr::Scalar(right)) => {
                        Expr::Scalar(left / right)
                    }
                    (Expr::Scalar(left), Opcode::Lt, Expr::Scalar(right)) => {
                        Expr::Scalar(left.lt(&right).into())
                    }
                    (Expr::Scalar(left), Opcode::Lte, Expr::Scalar(right)) => {
                        Expr::Scalar(left.le(&right).into())
                    }
                    (Expr::Scalar(left), Opcode::Gt, Expr::Scalar(right)) => {
                        Expr::Scalar(left.gt(&right).into())
                    }
                    (Expr::Scalar(left), Opcode::Gte, Expr::Scalar(right)) => {
                        Expr::Scalar(left.ge(&right).into())
                    }
                    (Expr::Scalar(left), Opcode::EqEq, Expr::Scalar(right)) => {
                        Expr::Scalar(left.eq(&right).into())
                    }
                    (Expr::Scalar(left), Opcode::Ne, Expr::Scalar(right)) => {
                        Expr::Scalar(left.ne(&right).into())
                    }
                    (left, op, right) => Expr::BinOp(Box::new(left), op, Box::new(right)),
                }
            }
            e => e,
        };
        Ok(expr)
    }

    fn visit_collection(self, collection: Collection) -> Result<Self::Value, Self::Error> {
        let expr = match collection {
            Collection::Array(array) => {
                let array = array
                    .into_iter()
                    .map(|e| e.accept(&*self))
                    .collect::<Result<Vec<Expr>, Self::Error>>()?;
                if array.iter().all(|e| matches!(e, Expr::Scalar(_))) {
                    let array = array
                        .into_iter()
                        .filter_map(|e| match e {
                            Expr::Scalar(value) => Some(value),
                            _ => None,
                        })
                        .collect();
                    Expr::Scalar(Value::Array(array))
                } else {
                    Expr::Collection(Collection::Array(array))
                }
            }
            Collection::Set(set) => {
                let set = set
                    .into_iter()
                    .map(|e| e.accept(&*self))
                    .collect::<Result<Vec<Expr>, Self::Error>>()?;
                if set.iter().all(|e| matches!(e, Expr::Scalar(_))) {
                    let set = set
                        .into_iter()
                        .filter_map(|e| match e {
                            Expr::Scalar(value) => Some(value),
                            _ => None,
                        })
                        .collect();
                    Expr::Scalar(Value::Set(set))
                } else {
                    Expr::Collection(Collection::Set(set))
                }
            }
            Collection::Object(obj) => {
                let obj = obj
                    .into_iter()
                    .map(|(key, val)| Ok((key.accept(&*self)?, val.accept(&*self)?)))
                    .collect::<Result<Vec<(Expr, Expr)>, Self::Error>>()?;
                if obj
                    .iter()
                    .all(|(k, v)| matches!(k, Expr::Scalar(_)) && matches!(v, Expr::Scalar(_)))
                {
                    let obj = obj
                        .into_iter()
                        .filter_map(|(k, v)| match (k, v) {
                            (Expr::Scalar(key), Expr::Scalar(value)) => Some((key, value)),
                            _ => None,
                        })
                        .collect();
                    Expr::Scalar(Value::Object(obj))
                } else {
                    Expr::Collection(Collection::Object(obj))
                }
            }
        };
        Ok(expr)
    }

    fn visit_comprehension(
        self,
        _comprehension: Comprehension,
    ) -> Result<Self::Value, Self::Error> {
        Ok(Expr::Scalar(Value::Undefined))
    }
}
