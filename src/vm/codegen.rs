use crate::ast::*;
use crate::value::Value;
use crate::vm::{BinOp, CollectType, Error, Instruction, INPUT_ROOT};

pub struct Codegen {
    instructions: Vec<Instruction>,
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    pub fn into_instructions(self) -> Vec<Instruction> {
        self.instructions
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

impl Visitor for Codegen {
    type Value = ();
    type Error = Error;

    fn visit_module(&mut self, _module: &mut Module) -> Result<Self::Value, Self::Error> {
        Ok(())
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Result<Self::Value, Self::Error> {
        match expr {
            Expr::Scalar(value) => self.instructions.push(Instruction::Const(value.clone())),
            Expr::Collection(collection) => self.push_collection(collection)?,
            Expr::Comprehension(compr) => self.push_comprehension(compr)?,
            // Root index into a global
            Expr::InputRoot => self.instructions.push(Instruction::LoadGlobal),
            // String index
            Expr::Var(var) => self
                .instructions
                .push(Instruction::Const(Value::String(var.clone()))),
            Expr::VarBrack(var) => self
                .instructions
                .push(Instruction::Const(Value::String(var.clone()))),
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
