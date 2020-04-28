use std::fmt;
use std::sync::Arc;

use crate::ast::*;
use crate::value::{Map, Set, Value};

mod operand;

use self::operand::Operand;

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
    pub fn op<'a>(&self, left: Operand<'a>, right: Operand<'a>) -> Operand<'a> {
        match self {
            BinOp::Add => left + right,
            BinOp::Sub => left - right,
            BinOp::Mul => left * right,
            BinOp::Div => left / right,
            BinOp::Lt => Operand::from_owned(left.lt(&right).into()),
            BinOp::Lte => Operand::from_owned(left.le(&right).into()),
            BinOp::Gt => Operand::from_owned(left.gt(&right).into()),
            BinOp::Gte => Operand::from_owned(left.ge(&right).into()),
            BinOp::EqEq => Operand::from_owned(left.eq(&right).into()),
            BinOp::Ne => Operand::from_owned(left.ne(&right).into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Instruction<'a> {
    Noop,
    Const(Value<'a>),
    BinOp(BinOp),
    AppendValue,
    InsertKeyValue,
    Terminate,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidNumArgs(usize, usize),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::InvalidNumArgs(expected, got) => write!(
                f,
                "invalid number of args for op. expected {}, got {}",
                expected, got
            ),
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
pub struct Machine<'a> {
    instructions: Arc<Vec<Instruction<'a>>>,
}

impl<'a> Machine<'a> {
    pub fn instance(&self) -> Instance<'a> {
        Instance {
            instructions: self.instructions.clone(),
            value_stack: Vec::with_capacity(10),
        }
    }
}

pub struct Instance<'m> {
    instructions: Arc<Vec<Instruction<'m>>>,
    value_stack: Vec<Operand<'m>>,
}

impl<'a> Instance<'a> {
    pub fn eval(&'a mut self) -> Result<Option<Value<'_>>, Error> {
        use Instruction::*;

        let mut pc = 0;
        self.value_stack.clear();

        while pc < self.instructions.len() {
            match self.instructions[pc] {
                Noop => (),
                Const(ref v) => self.value_stack.push(Operand::from_ref(v)),
                BinOp(binop) => {
                    let len = self.value_stack.len();
                    let right = self.value_stack.pop();
                    let left = self.value_stack.pop();
                    match (left, right) {
                        (Some(left), Some(right)) => {
                            let result = binop.op(left, right);
                            self.value_stack.push(result);
                        }
                        _ => return Err(Error::InvalidNumArgs(2, len)),
                    }
                }
                AppendValue => {
                    let len = self.value_stack.len();
                    let value = self.value_stack.pop();
                    let collection = self.value_stack.pop();
                    match (collection, value) {
                        (Some(mut collection), Some(value)) => {
                            collection.as_mut().push(value.into_value());
                            self.value_stack.push(collection);
                        }
                        _ => return Err(Error::InvalidNumArgs(2, len)),
                    }
                }
                InsertKeyValue => {
                    let len = self.value_stack.len();
                    let value = self.value_stack.pop();
                    let key = self.value_stack.pop();
                    let collection = self.value_stack.pop();
                    match (collection, key, value) {
                        (Some(mut collection), Some(key), Some(value)) => {
                            collection
                                .as_mut()
                                .insert(key.into_value(), value.into_value());
                            self.value_stack.push(collection);
                        }
                        _ => return Err(Error::InvalidNumArgs(3, len)),
                    }
                }
                Terminate => break,
            }
            pc += 1
        }
        Ok(self.value_stack.pop().map(|o| o.into_value()))
    }
}

pub struct Compiler<'input> {
    instructions: Vec<Instruction<'input>>,
}

impl<'input> Compiler<'input> {
    pub fn new() -> Self {
        Compiler {
            instructions: Vec::new(),
        }
    }
}

impl<'input> Visitor<'input> for &mut Compiler<'input> {
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
        Ok(())
    }

    fn visit_collection(self, collection: &Collection<'input>) -> Result<Self::Value, Self::Error> {
        match collection {
            Collection::Array(array) => {
                self.instructions
                    .push(Instruction::Const(Value::Array(vec![])));
                for term in array {
                    term.accept(&mut *self)?;
                    self.instructions.push(Instruction::AppendValue);
                }
            }
            Collection::Set(set) => {
                self.instructions
                    .push(Instruction::Const(Value::Set(Set::new())));
                for term in set {
                    term.accept(&mut *self)?;
                    self.instructions.push(Instruction::AppendValue);
                }
            }
            Collection::Object(obj) => {
                self.instructions
                    .push(Instruction::Const(Value::Object(Map::new())));
                for (key, value) in obj {
                    key.accept(&mut *self)?;
                    value.accept(&mut *self)?;
                    self.instructions.push(Instruction::InsertKeyValue);
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

    use crate::parser::parse_expr;

    #[test]
    fn eval() {
        let instructions = vec![
            Instruction::Const(Value::Number(3.into())),
            Instruction::Const(Value::Number(4.into())),
            Instruction::BinOp(BinOp::Add),
        ];

        let machine = Machine {
            instructions: Arc::new(instructions),
        };
        let mut instance = machine.instance();
        let result = instance.eval();
        println!("result: {:?}", result);
    }

    #[test]
    fn compile() {
        let input = "(3 + 4) == 3";
        let term = parse_expr(&input).unwrap();
        let mut compiler = Compiler::new();
        term.accept(&mut compiler).unwrap();

        let Compiler { instructions } = compiler;
        let machine = Machine {
            instructions: Arc::new(instructions),
        };
        let mut instance = machine.instance();
        let result = instance.eval();
        println!("result: {:?}", result);
    }

    #[test]
    fn test_array() {
        let input = "[[3, 2], 2, 3]";
        let term = parse_expr(&input).unwrap();
        let mut compiler = Compiler::new();
        term.accept(&mut compiler).unwrap();

        let Compiler { instructions } = compiler;
        let machine = Machine {
            instructions: Arc::new(instructions),
        };
        let mut instance = machine.instance();
        let result = instance.eval().unwrap().unwrap().try_into_array().unwrap();
        let expected: Vec<Value<'_>> = vec![
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
        let machine = Machine {
            instructions: Arc::new(instructions),
        };
        let mut instance = machine.instance();
        let result = instance.eval().unwrap().unwrap().try_into_set().unwrap();
        let mut expected: Set<Value<'_>> = Set::new();
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
        let machine = Machine {
            instructions: Arc::new(instructions),
        };
        let mut instance = machine.instance();
        let result = instance.eval().unwrap().unwrap().try_into_object().unwrap();
        let mut expected: Map<Value<'_>, Value<'_>> = Map::new();
        expected.insert("three".into(), 3.into());
        expected.insert("two".into(), 2.into());
        assert_eq!(expected, result);
    }
}
