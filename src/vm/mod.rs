use std::fmt;
use std::sync::Arc;

use typed_arena::Arena;

use crate::ast::*;
use crate::value::{Map, Set, Value};

mod codegen;
mod const_eval;
mod name_resolve;
mod stack;

pub use stack::Stack;

static UNDEFINED: Value = Value::Undefined;
const INPUT_ROOT: &str = "input";
const DATA_ROOT: &str = "data";

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
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
pub enum CollectType {
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
pub enum Instruction {
    LoadGlobal,
    LoadImmediate(Value),
    BinOp(BinOp),
    Collect(CollectType, usize),
    Index,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::LoadGlobal => write!(f, "loadg"),
            Self::LoadImmediate(v) => write!(f, "loadi {}", v),
            Self::BinOp(op) => write!(f, "binop {}", op),
            Self::Collect(ty, size) => write!(f, "collect {} {}", ty, size),
            Self::Index => write!(f, "index"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidNumArgs(usize, usize),
    InvalidValueType(&'static str),
    StackUnderflow,
    StackOverflow,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::InvalidNumArgs(expected, got) => write!(
                f,
                "invalid number of args for op. expected {}, got {}",
                expected, got
            ),
            Error::InvalidValueType(t) => {
                write!(f, "unexpected value type on stack: expected {}", t)
            }
            Error::StackUnderflow => write!(f, "stack underflow"),
            Error::StackOverflow => write!(f, "stack overflow"),
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
    pub fn from_query(mut expr: Expr) -> Result<Self, Error> {
        expr.accept(&mut const_eval::ConstEval)?;

        // name resolution
        let mut names = name_resolve::ModuleNameResolution::new(&vec![]);
        expr.accept(&mut names)?;

        let mut codegen = codegen::Codegen::new();
        expr.accept(&mut codegen)?;

        let query = CompiledQuery {
            instructions: Arc::new(codegen.into_instructions()),
        };
        Ok(query)
    }

    pub fn compile(mut expr: Expr, mut modules: Vec<Module>) -> Result<Self, Error> {
        let mut codegen = codegen::Codegen::new();

        for module in &mut modules {
            // Const Eval
            module.accept(&mut const_eval::ConstEval)?;

            // Name resolution
            let mut names = name_resolve::ModuleNameResolution::new(module.package());
            module.accept(&mut names)?;

            // Codegen
            module.accept(&mut codegen)?;
        }

        // Compile the query
        expr.accept(&mut const_eval::ConstEval)?;
        expr.accept(&mut codegen)?;

        let query = CompiledQuery {
            instructions: Arc::new(codegen.into_instructions()),
        };
        Ok(query)
    }

    pub fn eval(&self, input: Value) -> Result<Value, Error> {
        let mut instance = Instance {
            instructions: self.instructions.clone(),
            value_stack: Stack::new(),
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
    value_stack: Stack<&'m Value>,
    heap: Arena<Value>,
    input: Value,
}

impl<'a> Instance<'a> {
    fn eval(&'a mut self) -> Result<Value, Error> {
        use Instruction::*;

        let mut pc = 0;
        self.value_stack.clear();

        while pc < self.instructions.len() {
            match self.instructions[pc] {
                LoadGlobal => self.value_stack.push(&self.input)?,
                LoadImmediate(ref v) => self.value_stack.push(v)?,
                BinOp(binop) => {
                    let right = self.value_stack.pop()?;
                    let left = self.value_stack.pop()?;
                    let result = self.heap.alloc(binop.op(left, right));
                    self.value_stack.push(&*result)?;
                }
                Collect(ref ty, len) => match ty {
                    CollectType::Array => {
                        let mut result = vec![];
                        for _i in 0..len {
                            let value = self.value_stack.pop()?;
                            result.push(value.clone());
                        }
                        let result = self.heap.alloc(Value::Array(result));
                        self.value_stack.push(&*result)?;
                    }
                    CollectType::Set => {
                        let mut result = Set::new();
                        for _i in 0..len {
                            let value = self.value_stack.pop()?;
                            result.insert(value.clone());
                        }
                        let result = self.heap.alloc(Value::Set(result));
                        self.value_stack.push(&*result)?;
                    }
                    CollectType::Map => {
                        let mut result = Map::new();
                        for _i in 0..len {
                            let value = self.value_stack.pop()?;
                            let key = self.value_stack.pop()?;
                            result.insert(key.clone(), value.clone());
                        }
                        let result = self.heap.alloc(Value::Object(result));
                        self.value_stack.push(&*result)?;
                    }
                },
                Index => {
                    let arg = self.value_stack.pop()?;
                    let target = self.value_stack.pop()?;
                    let result = target.get(arg).unwrap_or_else(|| &UNDEFINED);
                    self.value_stack.push(result)?;
                }
            }
            pc += 1
        }
        self.value_stack.pop().map(|o| o.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::convert::TryFrom;

    use crate::parser::parse_query;

    #[test]
    fn eval() {
        let instructions = vec![
            Instruction::LoadImmediate(Value::Number(3.into())),
            Instruction::LoadImmediate(Value::Number(4.into())),
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
        let expr = Expr::try_from(term).unwrap();
        let query = CompiledQuery::from_query(expr).unwrap();
        println!("{}", query);
        let result = query.eval(Value::Undefined);
        println!("result: {:?}", result);
    }

    #[test]
    fn test_array() {
        let input = "[[3, 2], 2, 3]";
        let term = parse_query(&input).unwrap();
        let expr = Expr::try_from(term).unwrap();
        let query = CompiledQuery::from_query(expr).unwrap();
        println!("{}", query);
        let result = query
            .eval(Value::Undefined)
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
        let expr = Expr::try_from(term).unwrap();
        let query = CompiledQuery::from_query(expr).unwrap();
        println!("{}", query);
        let result = query
            .eval(Value::Undefined)
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
        let expr = Expr::try_from(term).unwrap();
        let query = CompiledQuery::from_query(expr).unwrap();
        println!("{}", query);
        let result = query
            .eval(Value::Undefined)
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
        let expr = Expr::try_from(term).unwrap();
        let query = CompiledQuery::from_query(expr).unwrap();
        let result = query.eval(Value::Undefined).unwrap();
        let expected = Value::Number(2.into());
        assert_eq!(expected, result);
    }

    #[test]
    fn test_object_index() {
        let input = "{\"three\": 3, \"two\": 2}[\"three\"]";
        let term = parse_query(&input).unwrap();
        let expr = Expr::try_from(term).unwrap();
        let query = CompiledQuery::from_query(expr).unwrap();
        let result = query.eval(Value::Undefined).unwrap();
        let expected = Value::Number(3.into());
        assert_eq!(expected, result);
    }

    #[test]
    fn test_data_input() {
        let query = "input.a == 3";
        let term = parse_query(&query).unwrap();
        let expr = Expr::try_from(term).unwrap();
        let query = CompiledQuery::from_query(expr).unwrap();
        let mut input = Map::new();
        input.insert(Value::String("a".to_string()), Value::Number(3.into()));
        let result = query.eval(Value::Object(input)).unwrap();
        let expected = Value::Bool(true);
        assert_eq!(expected, result);
    }
}
