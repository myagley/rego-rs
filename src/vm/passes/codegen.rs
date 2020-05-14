use std::collections::HashMap;
use std::fmt;

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

    pub fn push_label(&mut self, label: &str) {
        self.labels
            .insert(label.to_string(), self.instructions.len());
    }

    pub fn push_ir(&mut self, ir: Ir) {
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
                Ir::LoadLocal(value) => Ok(Instruction::LoadLocal(value)),
                Ir::StoreLocal(value) => Ok(Instruction::StoreLocal(value)),
                Ir::Collect(ty, num) => Ok(Instruction::Collect(ty, num)),
                Ir::Index => Ok(Instruction::Index),
                Ir::Dup => Ok(Instruction::Dup),
                Ir::Pop => Ok(Instruction::Pop),
                Ir::BinOp(op) => Ok(Instruction::BinOp(op)),
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
                Ir::BranchTrue(offset) => Ok(Instruction::BranchTrue(offset)),
            })
            .collect::<Result<Vec<Instruction>, Error>>()
    }

    fn push_op(&mut self, opcode: Opcode) {
        match opcode {
            Opcode::Add => self.instructions.push(Ir::BinOp(BinOp::Add)),
            Opcode::Sub => self.instructions.push(Ir::BinOp(BinOp::Sub)),
            Opcode::Mul => self.instructions.push(Ir::BinOp(BinOp::Mul)),
            Opcode::Div => self.instructions.push(Ir::BinOp(BinOp::Div)),
            Opcode::And => self.instructions.push(Ir::BinOp(BinOp::And)),
            Opcode::Or => self.instructions.push(Ir::BinOp(BinOp::Or)),
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

    fn push_comprehension(&mut self, _comprehension: &mut Comprehension) -> Result<(), Error> {
        Ok(())
    }

    fn push_complete_clause(
        &mut self,
        name: &str,
        clause: &mut CompleteClause,
    ) -> Result<(), Error> {
        match &mut clause.body {
            ClauseBody::Query(expr) => {
                self.push_label(name);
                expr.accept(self)?;
                self.push_ir(Ir::BranchTrue(3));
                self.push_ir(Ir::LoadImmediate(Value::Undefined));
                self.push_ir(Ir::Return);
                clause.value.accept(self)?;
                self.push_ir(Ir::Return);
            }
            ClauseBody::WithElses(expr, elses) => {
                self.push_label(&format!("{}-{}", name, 0));
                expr.accept(self)?;
                self.push_ir(Ir::BranchTrue(3));
                self.push_ir(Ir::LoadImmediate(Value::Undefined));
                self.push_ir(Ir::Return);
                clause.value.accept(self)?;
                self.push_ir(Ir::Return);

                for (i, el) in elses.iter_mut().enumerate() {
                    self.push_label(&format!("{}-{}", name, i + 1));
                    el.query_mut().accept(self)?;
                    self.push_ir(Ir::BranchTrue(3));
                    self.push_ir(Ir::LoadImmediate(Value::Undefined));
                    self.push_ir(Ir::Return);
                    if let Some(value) = el.value_mut() {
                        value.accept(self)?;
                    } else {
                        clause.value.accept(self)?;
                    }
                    self.push_ir(Ir::Return);
                }

                self.push_label(name);
                for i in 0..elses.len() + 1 {
                    self.push_ir(Ir::Call(format!("{}-{}", name, i)));
                    self.push_ir(Ir::Dup);
                    self.push_ir(Ir::BranchUndefined(2));
                    self.push_ir(Ir::Return);
                    self.push_ir(Ir::Pop);
                }
                self.push_ir(Ir::LoadImmediate(Value::Undefined));
                self.push_ir(Ir::Return);
            }
        }
        Ok(())
    }

    fn push_complete_rule(
        &mut self,
        name: &str,
        clauses: &mut [CompleteClause],
    ) -> Result<(), Error> {
        match clauses {
            &mut [] => self.push_label(name),
            // TODO(miyagley) - optimize for single clause?
            // &mut [ref mut clause] => self.push_complete_clause(&name, clause)?,
            clauses => {
                // Push each clause with a unique label
                for (i, clause) in clauses.iter_mut().enumerate() {
                    self.push_complete_clause(&format!("$__{}-{}", name, i), clause)?;
                }

                // Call each clause in sequence and early return if defined
                self.push_label(name);
                for (i, _clause) in clauses.iter_mut().enumerate() {
                    self.push_ir(Ir::Call(format!("$__{}-{}", name, i)));
                    self.push_ir(Ir::Dup);
                    self.push_ir(Ir::BranchUndefined(2));
                    self.push_ir(Ir::Return);
                    self.push_ir(Ir::Pop);
                }
            }
        }
        Ok(())
    }
}

impl Visitor for Codegen {
    type Value = ();
    type Error = Error;

    fn visit_module(&mut self, module: &mut Module) -> Result<Self::Value, Self::Error> {
        for rule in module.rules_mut() {
            match &mut rule.body {
                Body::Complete(clauses) => {
                    self.push_complete_rule(&rule.name, clauses.as_mut_slice())?
                }
                _ => todo!(),
            }

            // Handle the default
            if let Some(default) = rule.default.as_mut() {
                default.accept(self)?;
            } else {
                self.instructions.push(Ir::LoadImmediate(Value::Undefined));
            }
            self.instructions.push(Ir::Return);
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
            Expr::Var(var) => self.instructions.push(Ir::LoadLocal(var.clone())),
            Expr::VarBrack(var) => self.instructions.push(Ir::LoadLocal(var.clone())),
            Expr::BinOp(left, op, right) => {
                left.accept(self)?;
                right.accept(self)?;
                self.push_op(*op);
            }
            Expr::Assign(var, expr) => {
                expr.accept(self)?;
                self.instructions.push(Ir::StoreLocal(var.clone()));
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

impl fmt::Display for Codegen {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let label_lookup: HashMap<usize, &str> =
            self.labels.iter().map(|(k, v)| (*v, k.as_str())).collect();
        for (i, instruction) in self.instructions.iter().enumerate() {
            if let Some(label) = label_lookup.get(&i) {
                writeln!(f, "       {}", label)?;
            }
            writeln!(f, "{:>5}: {}", i, instruction)?;
        }
        Ok(())
    }
}
