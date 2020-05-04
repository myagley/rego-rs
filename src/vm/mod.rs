use std::convert::TryFrom;
use std::fmt;
use std::sync::Arc;

use typed_arena::Arena;

use crate::ast::*;
use crate::parser::tree::Query;
use crate::value::{Map, Set, Value};

mod const_eval;

static UNDEFINED: Value = Value::Undefined;

#[derive(Debug, Clone, Copy, PartialEq)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Lte,
    Gt,
    Gte,
    EqEq,
    Ne,
}

impl BinOp {
    pub fn op(&self, left: &Value, right: &Value) -> Value {
        match self {
            BinOp::Add => left + right,
            BinOp::Sub => left - right,
            BinOp::Mul => left * right,
            BinOp::Div => left / right,
            BinOp::Lt => left.lt(&right).into(),
            BinOp::Lte => left.le(&right).into(),
            BinOp::Gt => left.gt(&right).into(),
            BinOp::Gte => left.ge(&right).into(),
            BinOp::EqEq => left.eq(&right).into(),
            BinOp::Ne => left.ne(&right).into(),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "add"),
            BinOp::Sub => write!(f, "sub"),
            BinOp::Mul => write!(f, "mul"),
            BinOp::Div => write!(f, "div"),
            BinOp::Lt => write!(f, "lt"),
            BinOp::Lte => write!(f, "lte"),
            BinOp::Gt => write!(f, "gt"),
            BinOp::Gte => write!(f, "gte"),
            BinOp::EqEq => write!(f, "eqeq"),
            BinOp::Ne => write!(f, "ne"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum CollectType {
    Array,
    Set,
    Map,
}

impl fmt::Display for CollectType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Array => write!(f, "arr"),
            Self::Set => write!(f, "set"),
            Self::Map => write!(f, "map"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Instruction {
    Const(Value),
    BinOp(BinOp),
    Collect(CollectType, usize),
    Index,
    LoadGlobal,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Const(v) => write!(f, "const {}", v),
            Self::BinOp(op) => write!(f, "binop {}", op),
            Self::Collect(ty, size) => write!(f, "collect {} {}", ty, size),
            Self::Index => write!(f, "index"),
            Self::LoadGlobal => write!(f, "loadg"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidNumArgs(usize, usize),
    StackUnderflow,
    InvalidValueType(&'static str),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::InvalidNumArgs(expected, got) => write!(
                f,
                "invalid number of args for op. expected {}, got {}",
                expected, got
            ),
            Error::StackUnderflow => write!(f, "stack underflow"),
            Error::InvalidValueType(t) => {
                write!(f, "unexpected value type on stack: expected {}", t)
            }
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct CompiledQuery {
    instructions: Arc<Vec<Instruction>>,
}

impl CompiledQuery {
    pub fn from_query(query: Query<'_>) -> Result<Self, Error> {
        let mut expr = Expr::try_from(query).unwrap();
        // expr.accept(&mut const_eval::ConstEval)?;
        let mut compiler = Compiler::new();
        expr.accept(&mut compiler)?;

        let Compiler { instructions } = compiler;
        let query = CompiledQuery {
            instructions: Arc::new(instructions),
        };
        Ok(query)
    }

    pub fn eval(&self, input: Value) -> Result<Option<Value>, Error> {
        let mut instance = Instance {
            instructions: self.instructions.clone(),
            value_stack: Vec::with_capacity(10),
            heap: Arena::new(),
            input,
        };
        instance.eval()
    }
}

impl fmt::Display for CompiledQuery {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for i in self.instructions.as_ref() {
            writeln!(f, "{}", i)?
        }
        Ok(())
    }
}

struct Instance<'m> {
    instructions: Arc<Vec<Instruction>>,
    value_stack: Vec<&'m Value>,
    heap: Arena<Value>,
    input: Value,
}

impl<'a> Instance<'a> {
    fn eval(&'a mut self) -> Result<Option<Value>, Error> {
        use Instruction::*;

        let mut pc = 0;
        self.value_stack.clear();

        while pc < self.instructions.len() {
            match self.instructions[pc] {
                Const(ref v) => self.value_stack.push(v),
                BinOp(binop) => {
                    let right = self
                        .value_stack
                        .pop()
                        .ok_or_else(|| Error::StackUnderflow)?;
                    let left = self
                        .value_stack
                        .pop()
                        .ok_or_else(|| Error::StackUnderflow)?;
                    let result = self.heap.alloc(binop.op(left, right));
                    self.value_stack.push(&*result);
                }
                Collect(ref ty, len) => match ty {
                    CollectType::Array => {
                        let mut result = vec![];
                        for _i in 0..len {
                            let value = self
                                .value_stack
                                .pop()
                                .ok_or_else(|| Error::StackUnderflow)?;
                            result.push(value.clone());
                        }
                        let result = self.heap.alloc(Value::Array(result));
                        self.value_stack.push(&*result);
                    }
                    CollectType::Set => {
                        let mut result = Set::new();
                        for _i in 0..len {
                            let value = self
                                .value_stack
                                .pop()
                                .ok_or_else(|| Error::StackUnderflow)?;
                            result.insert(value.clone());
                        }
                        let result = self.heap.alloc(Value::Set(result));
                        self.value_stack.push(&*result);
                    }
                    CollectType::Map => {
                        let mut result = Map::new();
                        for _i in 0..len {
                            let value = self
                                .value_stack
                                .pop()
                                .ok_or_else(|| Error::StackUnderflow)?;
                            let key = self
                                .value_stack
                                .pop()
                                .ok_or_else(|| Error::StackUnderflow)?;
                            result.insert(key.clone(), value.clone());
                        }
                        let result = self.heap.alloc(Value::Object(result));
                        self.value_stack.push(&*result);
                    }
                },
                Index => {
                    let arg = self
                        .value_stack
                        .pop()
                        .ok_or_else(|| Error::StackUnderflow)?;
                    let target = self
                        .value_stack
                        .pop()
                        .ok_or_else(|| Error::StackUnderflow)?;
                    let result = target.get(arg).unwrap_or_else(|| &UNDEFINED);
                    self.value_stack.push(result);
                }
                LoadGlobal => {
                    self.value_stack.push(&self.input);
                }
            }
            pc += 1
        }
        Ok(self.value_stack.pop().map(|o| o.clone()))
    }
}

pub struct Compiler {
    instructions: Vec<Instruction>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Vec::new(),
        }
    }

    fn push_op(&mut self, opcode: Opcode) {
        match opcode {
            Opcode::Add => self.instructions.push(Instruction::BinOp(BinOp::Add)),
            Opcode::Sub => self.instructions.push(Instruction::BinOp(BinOp::Sub)),
            Opcode::Mul => self.instructions.push(Instruction::BinOp(BinOp::Mul)),
            Opcode::Div => self.instructions.push(Instruction::BinOp(BinOp::Div)),
            Opcode::Lt => self.instructions.push(Instruction::BinOp(BinOp::Lt)),
            Opcode::Lte => self.instructions.push(Instruction::BinOp(BinOp::Lte)),
            Opcode::Gt => self.instructions.push(Instruction::BinOp(BinOp::Gt)),
            Opcode::Gte => self.instructions.push(Instruction::BinOp(BinOp::Gte)),
            Opcode::EqEq => self.instructions.push(Instruction::BinOp(BinOp::EqEq)),
            Opcode::Ne => self.instructions.push(Instruction::BinOp(BinOp::Ne)),
            _ => todo!(),
        }
    }

    fn push_collection(&mut self, collection: &mut Collection) -> Result<(), Error> {
        match collection {
            Collection::Array(array) => {
                let len = array.len();

                // push the terms
                for term in array.into_iter().rev() {
                    term.accept(self)?;
                }

                // push the collect instruction
                self.instructions
                    .push(Instruction::Collect(CollectType::Array, len));
            }
            Collection::Set(set) => {
                let len = set.len();

                // push the terms
                for term in set {
                    term.accept(self)?;
                }

                // push the collect instruction
                self.instructions
                    .push(Instruction::Collect(CollectType::Set, len));
            }
            Collection::Object(obj) => {
                let len = obj.len();

                // push the terms
                for (key, value) in obj {
                    key.accept(self)?;
                    value.accept(self)?;
                }

                // push the collect instruction
                self.instructions
                    .push(Instruction::Collect(CollectType::Map, len));
            }
        }
        Ok(())
    }

    fn push_comprehension(&mut self, _comprehension: &mut Comprehension) -> Result<(), Error> {
        Ok(())
    }
}

impl Visitor for Compiler {
    type Value = ();
    type Error = Error;

    fn visit_expr(&mut self, expr: &mut Expr) -> Result<Self::Value, Self::Error> {
        match expr {
            Expr::Scalar(value) => self.instructions.push(Instruction::Const(value.clone())),
            Expr::Collection(collection) => self.push_collection(collection)?,
            Expr::Comprehension(compr) => self.push_comprehension(compr)?,
            Expr::Var(ref s) if s == &"input" => self.instructions.push(Instruction::LoadGlobal),
            Expr::BinOp(left, op, right) => {
                left.accept(self)?;
                right.accept(self)?;
                self.push_op(*op);
            }
            Expr::Index(refs) => {
                let mut iter = refs.into_iter();
                if let Some(head) = iter.next() {
                    head.accept(self)?;
                    for next in iter {
                        next.accept(self)?;
                        self.instructions.push(Instruction::Index);
                    }
                }
            }
            _ => todo!(),
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::parse_query;

    #[test]
    fn eval() {
        let instructions = vec![
            Instruction::Const(Value::Number(3.into())),
            Instruction::Const(Value::Number(4.into())),
            Instruction::BinOp(BinOp::Add),
        ];

        let query = CompiledQuery {
            instructions: Arc::new(instructions),
        };
        let result = query.eval(Value::Undefined);
        println!("result: {:?}", result);
    }

    #[test]
    fn compile() {
        let input = "(3 + 4) == 3";
        let term = parse_query(&input).unwrap();
        let query = CompiledQuery::from_query(term).unwrap();
        println!("{}", query);
        let result = query.eval(Value::Undefined);
        println!("result: {:?}", result);
    }

    #[test]
    fn test_array() {
        let input = "[[3, 2], 2, 3]";
        let term = parse_query(&input).unwrap();
        let query = CompiledQuery::from_query(term).unwrap();
        println!("{}", query);
        let result = query
            .eval(Value::Undefined)
            .unwrap()
            .unwrap()
            .try_into_array()
            .unwrap();
        let expected: Vec<Value> = vec![
            Value::Array(vec![Value::Number(3.into()), Value::Number(2.into())]),
            2.into(),
            3.into(),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn test_set() {
        let input = "{[3, 2], 2, 3}";
        let term = parse_query(&input).unwrap();
        let query = CompiledQuery::from_query(term).unwrap();
        println!("{}", query);
        let result = query
            .eval(Value::Undefined)
            .unwrap()
            .unwrap()
            .try_into_set()
            .unwrap();
        let mut expected: Set<Value> = Set::new();
        expected.insert(Value::Array(vec![
            Value::Number(3.into()),
            Value::Number(2.into()),
        ]));
        expected.insert(2.into());
        expected.insert(3.into());
        assert_eq!(expected, result);
    }

    #[test]
    fn test_object() {
        let input = "{\"three\": 3, \"two\": 2}";
        let term = parse_query(&input).unwrap();
        let query = CompiledQuery::from_query(term).unwrap();
        println!("{}", query);
        let result = query
            .eval(Value::Undefined)
            .unwrap()
            .unwrap()
            .try_into_object()
            .unwrap();
        let mut expected: Map<Value, Value> = Map::new();
        expected.insert("three".into(), 3.into());
        expected.insert("two".into(), 2.into());
        assert_eq!(expected, result);
    }

    #[test]
    fn test_array_index() {
        let input = "[[3, 2], 2, 3][0][1]";
        let term = parse_query(&input).unwrap();
        let query = CompiledQuery::from_query(term).unwrap();
        let result = query.eval(Value::Undefined).unwrap().unwrap();
        let expected = Value::Number(2.into());
        assert_eq!(expected, result);
    }

    #[test]
    fn test_object_index() {
        let input = "{\"three\": 3, \"two\": 2}[\"three\"]";
        let term = parse_query(&input).unwrap();
        let query = CompiledQuery::from_query(term).unwrap();
        let result = query.eval(Value::Undefined).unwrap().unwrap();
        let expected = Value::Number(3.into());
        assert_eq!(expected, result);
    }

    #[test]
    fn test_data_input() {
        let query = "input.a == 3";
        let term = parse_query(&query).unwrap();
        let query = CompiledQuery::from_query(term).unwrap();
        let mut input = Map::new();
        input.insert(Value::String("a".to_string()), Value::Number(3.into()));
        let result = query.eval(Value::Object(input)).unwrap().unwrap();
        let expected = Value::Bool(true);
        assert_eq!(expected, result);
    }
}
