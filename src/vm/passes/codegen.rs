use std::collections::HashMap;

use crate::ast::*;
use crate::value::Value;
use crate::vm::{BinOp, CollectType, Error, Instruction, Ir};

/// Converts AST to a vec of instructions
pub struct Codegen {
    instructions: Vec<Ir>,
    labels: HashMap<String, usize>,
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            labels: HashMap::new(),
        }
    }

    pub fn push_ir(&mut self, ir: Ir) {
        if let Ir::Label(ref label) = ir {
            self.labels
                .insert(label.to_string(), self.instructions.len());
        }
        self.instructions.push(ir);
    }

    pub fn assemble(self) -> Result<Vec<Instruction>, Error> {
        let labels = self.labels;
        let instructions = self.instructions;
        instructions
            .into_iter()
            .map(move |ir| match ir {
                Ir::LoadGlobal => Ok(Instruction::LoadGlobal),
                Ir::LoadImmediate(value) => Ok(Instruction::LoadImmediate(value)),
                Ir::Collect(ty, num) => Ok(Instruction::Collect(ty, num)),
                Ir::Index => Ok(Instruction::Index),
                Ir::Dup => Ok(Instruction::Dup),
                Ir::Pop => Ok(Instruction::Pop),
                Ir::BinOp(op) => Ok(Instruction::BinOp(op)),
                Ir::Label(label) => Ok(Instruction::Label(label)),
                Ir::Call(label) => labels
                    .get(&label)
                    .map(|pc| Instruction::Call(*pc))
                    .ok_or_else(|| Error::UnknownReference(label)),
                Ir::Return => Ok(Instruction::Return),
                Ir::Jump(label) => labels
                    .get(&label)
                    .map(|pc| Instruction::Jump(*pc))
                    .ok_or_else(|| Error::UnknownReference(label)),
                Ir::BranchDefined(offset) => Ok(Instruction::BranchDefined(offset)),
                Ir::BranchUndefined(offset) => Ok(Instruction::BranchUndefined(offset)),
            })
            .collect::<Result<Vec<Instruction>, Error>>()
    }

    fn push_op(&mut self, opcode: Opcode) {
        match opcode {
            Opcode::Add => self.instructions.push(Ir::BinOp(BinOp::Add)),
            Opcode::Sub => self.instructions.push(Ir::BinOp(BinOp::Sub)),
            Opcode::Mul => self.instructions.push(Ir::BinOp(BinOp::Mul)),
            Opcode::Div => self.instructions.push(Ir::BinOp(BinOp::Div)),
            Opcode::Lt => self.instructions.push(Ir::BinOp(BinOp::Lt)),
            Opcode::Lte => self.instructions.push(Ir::BinOp(BinOp::Lte)),
            Opcode::Gt => self.instructions.push(Ir::BinOp(BinOp::Gt)),
            Opcode::Gte => self.instructions.push(Ir::BinOp(BinOp::Gte)),
            Opcode::EqEq => self.instructions.push(Ir::BinOp(BinOp::EqEq)),
            Opcode::Ne => self.instructions.push(Ir::BinOp(BinOp::Ne)),
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
                self.instructions.push(Ir::Collect(CollectType::Array, len));
            }
            Collection::Set(set) => {
                let len = set.len();

                // push the terms
                for term in set {
                    term.accept(self)?;
                }

                // push the collect instruction
                self.instructions.push(Ir::Collect(CollectType::Set, len));
            }
            Collection::Object(obj) => {
                let len = obj.len();

                // push the terms
                for (key, value) in obj {
                    key.accept(self)?;
                    value.accept(self)?;
                }

                // push the collect instruction
                self.instructions.push(Ir::Collect(CollectType::Map, len));
            }
        }
        Ok(())
    }

    fn push_label(&mut self, label: &str) {
        self.push_ir(Ir::Label(label.to_string()));
    }

    fn push_comprehension(&mut self, _comprehension: &mut Comprehension) -> Result<(), Error> {
        Ok(())
    }

    fn push_clausebody(&mut self, body: &mut ClauseBody) -> Result<(), Error> {
        match body {
            ClauseBody::Query(expr) => expr.accept(self)?,
            _ => todo!(),
        }
        Ok(())
    }
}

impl Visitor for Codegen {
    type Value = ();
    type Error = Error;

    fn visit_module(&mut self, module: &mut Module) -> Result<Self::Value, Self::Error> {
        for rule in module.rules_mut() {
            if rule.body.len() > 0 {
                // match &mut rule.body {
                //     Body::Complete(clauses) => {
                //
                //     }
                //     _ => todo!(),
                // }
                // for (i, clause) in rule.clauses_mut().iter_mut().enumerate() {
                //     self.push_label(&format!("{}-{}", rule.name(), i));
                // }
                // self.instructions.push(Ir::Return);
            } else {
                self.push_label(&rule.name);
                // No clauses. Codegen the default or undefined
                if let Some(default) = rule.default.as_mut() {
                    default.accept(self)?;
                } else {
                    self.instructions.push(Ir::LoadImmediate(Value::Undefined));
                }
                self.instructions.push(Ir::Return);
            }
        }
        Ok(())
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Result<Self::Value, Self::Error> {
        match expr {
            Expr::Scalar(value) => self.instructions.push(Ir::LoadImmediate(value.clone())),
            Expr::Collection(collection) => self.push_collection(collection)?,
            Expr::Comprehension(compr) => self.push_comprehension(compr)?,
            // Root index into a global
            Expr::InputRoot => self.instructions.push(Ir::LoadGlobal),
            // String index
            // Expr::Var(var) => self
            //     .instructions
            //     .push(Ir::LoadImmediate(Value::String(var.clone()))),
            // Expr::VarBrack(var) => self
            //     .instructions
            //     .push(Ir::LoadImmediate(Value::String(var.clone()))),
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
                        self.instructions.push(Ir::Index);
                    }
                }
            }
            Expr::RuleCall(rule) => self.instructions.push(Ir::Call(rule.to_string())),
            e => {
                println!("{:?}", e);
                todo!()
            }
        }
        Ok(())
    }
}
