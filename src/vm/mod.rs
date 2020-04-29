use std::fmt;
use std::sync::Arc;

use typed_arena::Arena;

use crate::ast::*;
use crate::value::{Map, Set, Value};

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

#[derive(Debug, Clone, PartialEq)]
enum CollectType {
    Array,
    Set,
    Map,
}

#[derive(Debug, Clone, PartialEq)]
enum Instruction {
    Const(Value),
    BinOp(BinOp),
    Collect(CollectType),
    Index,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidNumArgs(usize, usize),
    InvalidStack,
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
            Error::InvalidStack => write!(f, "expected item on stack."),
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
    pub fn eval(&self) -> Result<Option<Value>, Error> {
        let mut instance = Instance {
            instructions: self.instructions.clone(),
            value_stack: Vec::with_capacity(10),
            heap: Arena::new(),
        };
        instance.eval()
    }
}

struct Instance<'m> {
    instructions: Arc<Vec<Instruction>>,
    value_stack: Vec<&'m Value>,
    heap: Arena<Value>,
}

impl<'a> Instance<'a> {
    pub fn eval(&'a mut self) -> Result<Option<Value>, Error> {
        use Instruction::*;

        let mut pc = 0;
        self.value_stack.clear();

        while pc < self.instructions.len() {
            match self.instructions[pc] {
                Const(ref v) => self.value_stack.push(v),
                BinOp(binop) => {
                    let right = self.value_stack.pop().ok_or_else(|| Error::InvalidStack)?;
                    let left = self.value_stack.pop().ok_or_else(|| Error::InvalidStack)?;
                    let result = self.heap.alloc(binop.op(left, right));
                    self.value_stack.push(&*result);
                }
                Collect(ref ty) => {
                    let len = self
                        .value_stack
                        .pop()
                        .ok_or_else(|| Error::InvalidStack)?
                        .as_u64()
                        .ok_or_else(|| Error::InvalidValueType("u64"))?;

                    match ty {
                        CollectType::Array => {
                            let mut result = vec![];
                            for _i in 0..len {
                                let value =
                                    self.value_stack.pop().ok_or_else(|| Error::InvalidStack)?;
                                result.push(value.clone());
                            }
                            let result = self.heap.alloc(Value::Array(result));
                            self.value_stack.push(&*result);
                        }
                        CollectType::Set => {
                            let mut result = Set::new();
                            for _i in 0..len {
                                let value =
                                    self.value_stack.pop().ok_or_else(|| Error::InvalidStack)?;
                                result.insert(value.clone());
                            }
                            let result = self.heap.alloc(Value::Set(result));
                            self.value_stack.push(&*result);
                        }
                        CollectType::Map => {
                            let mut result = Map::new();
                            for _i in 0..len {
                                let value =
                                    self.value_stack.pop().ok_or_else(|| Error::InvalidStack)?;
                                let key =
                                    self.value_stack.pop().ok_or_else(|| Error::InvalidStack)?;
                                result.insert(key.clone(), value.clone());
                            }
                            let result = self.heap.alloc(Value::Object(result));
                            self.value_stack.push(&*result);
                        }
                    }
                }
                Index => {
                    let arg = self.value_stack.pop().ok_or_else(|| Error::InvalidStack)?;
                    let target = self.value_stack.pop().ok_or_else(|| Error::InvalidStack)?;
                    let result = target.get(arg).unwrap_or_else(|| &UNDEFINED);
                    self.value_stack.push(result);
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
}

impl<'input> Visitor<'input> for &mut Compiler {
    type Value = ();
    type Error = Error;

    fn visit_term(self, term: &Term<'input>) -> Result<Self::Value, Self::Error> {
        match term {
            Term::BinOp(left, op, right) => {
                left.accept(&mut *self)?;
                right.accept(&mut *self)?;
                op.accept(&mut *self)?;
            }
            Term::Scalar(value) => self.instructions.push(Instruction::Const(value.clone())),
            Term::Ref(r) => r.accept(&mut *self)?,
        }
        Ok(())
    }

    fn visit_opcode(self, opcode: &Opcode) -> Result<Self::Value, Self::Error> {
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
        Ok(())
    }

    fn visit_ref(self, r: &Ref<'input>) -> Result<Self::Value, Self::Error> {
        r.target().accept(&mut *self)?;
        for arg in r.args() {
            arg.accept(&mut *self)?;
        }
        Ok(())
    }

    fn visit_ref_target(self, target: &RefTarget<'input>) -> Result<Self::Value, Self::Error> {
        match target {
            RefTarget::Collection(c) => c.accept(&mut *self)?,
            _ => todo!(),
        }
        Ok(())
    }

    fn visit_ref_arg(self, arg: &RefArg<'input>) -> Result<Self::Value, Self::Error> {
        match arg {
            RefArg::Scalar(value) => self.instructions.push(Instruction::Const(value.clone())),
            _ => todo!(),
        }
        self.instructions.push(Instruction::Index);
        Ok(())
    }

    fn visit_collection(self, collection: &Collection<'input>) -> Result<Self::Value, Self::Error> {
        match collection {
            Collection::Array(array) => {
                let len = array.len();

                // push the terms
                for term in array.iter().rev() {
                    term.accept(&mut *self)?;
                }
                // push the length
                self.instructions
                    .push(Instruction::Const(Value::Number(len.into())));

                // push the collect instruction
                self.instructions
                    .push(Instruction::Collect(CollectType::Array));
            }
            Collection::Set(set) => {
                let len = set.len();

                // push the terms
                for term in set {
                    term.accept(&mut *self)?;
                }
                // push the length
                self.instructions
                    .push(Instruction::Const(Value::Number(len.into())));

                // push the collect instruction
                self.instructions
                    .push(Instruction::Collect(CollectType::Set));
            }
            Collection::Object(obj) => {
                let len = obj.len();

                // push the terms
                for (key, value) in obj {
                    key.accept(&mut *self)?;
                    value.accept(&mut *self)?;
                }

                // push the length
                self.instructions
                    .push(Instruction::Const(Value::Number(len.into())));

                // push the collect instruction
                self.instructions
                    .push(Instruction::Collect(CollectType::Map));
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::parse_expr;

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
        let result = query.eval();
        println!("result: {:?}", result);
    }

    #[test]
    fn compile() {
        let input = "(3 + 4) == 3";
        let term = parse_expr(&input).unwrap();
        let mut compiler = Compiler::new();
        term.accept(&mut compiler).unwrap();

        let Compiler { instructions } = compiler;
        let query = CompiledQuery {
            instructions: Arc::new(instructions),
        };
        let result = query.eval();
        println!("result: {:?}", result);
    }

    #[test]
    fn test_array() {
        let input = "[[3, 2], 2, 3]";
        let term = parse_expr(&input).unwrap();
        let mut compiler = Compiler::new();
        term.accept(&mut compiler).unwrap();

        let Compiler { instructions } = compiler;
        let query = CompiledQuery {
            instructions: Arc::new(instructions),
        };
        let result = query.eval().unwrap().unwrap().try_into_array().unwrap();
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
        let term = parse_expr(&input).unwrap();
        let mut compiler = Compiler::new();
        term.accept(&mut compiler).unwrap();

        let Compiler { instructions } = compiler;
        let query = CompiledQuery {
            instructions: Arc::new(instructions),
        };
        let result = query.eval().unwrap().unwrap().try_into_set().unwrap();
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
        let term = parse_expr(&input).unwrap();
        let mut compiler = Compiler::new();
        term.accept(&mut compiler).unwrap();

        let Compiler { instructions } = compiler;
        let query = CompiledQuery {
            instructions: Arc::new(instructions),
        };
        let result = query.eval().unwrap().unwrap().try_into_object().unwrap();
        let mut expected: Map<Value, Value> = Map::new();
        expected.insert("three".into(), 3.into());
        expected.insert("two".into(), 2.into());
        assert_eq!(expected, result);
    }

    #[test]
    fn test_array_index() {
        let input = "[[3, 2], 2, 3][0][1]";
        let term = parse_expr(&input).unwrap();
        let mut compiler = Compiler::new();
        term.accept(&mut compiler).unwrap();

        let Compiler { instructions } = compiler;
        let query = CompiledQuery {
            instructions: Arc::new(instructions),
        };
        let result = query.eval().unwrap().unwrap();
        let expected = Value::Number(2.into());
        assert_eq!(expected, result);
    }

    #[test]
    fn test_object_index() {
        let input = "{\"three\": 3, \"two\": 2}[\"three\"]";
        let term = parse_expr(&input).unwrap();
        let mut compiler = Compiler::new();
        term.accept(&mut compiler).unwrap();

        let Compiler { instructions } = compiler;
        let query = CompiledQuery {
            instructions: Arc::new(instructions),
        };
        let result = query.eval().unwrap().unwrap();
        let expected = Value::Number(3.into());
        assert_eq!(expected, result);
    }
}
