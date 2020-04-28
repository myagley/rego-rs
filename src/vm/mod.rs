use std::sync::Arc;

use crate::ast::{Opcode, Term, Visitor};
use crate::value::Value;

mod operand;

use self::operand::Operand;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl Op {
    pub fn op<'a>(&self, left: Operand<'a>, right: Operand<'a>) -> Operand<'a> {
        match self {
            Op::Add => left + right,
            Op::Sub => left - right,
            Op::Mul => left * right,
            Op::Div => left / right,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Instruction<'a> {
    Noop,
    Const(Value<'a>),
    Op(Op),
    Terminate,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    Trap,
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
                Op(op) => {
                    let left = self.value_stack.pop();
                    let right = self.value_stack.pop();
                    match (left, right) {
                        (Some(left), Some(right)) => {
                            let result = op.op(left, right);
                            self.value_stack.push(result);
                        }
                        _ => return Err(Error::Trap),
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

    fn visit_term(self, term: Term<'input>) -> Result<Self::Value, Self::Error> {
        match term {
            Term::BinOp(left, op, right) => {
                left.accept(&mut *self)?;
                right.accept(&mut *self)?;
                op.accept(&mut *self)?;
            }
            Term::Scalar(value) => self.instructions.push(Instruction::Const(value)),
            _ => todo!(),
        }
        Ok(())
    }

    fn visit_opcode(self, opcode: Opcode) -> Result<Self::Value, Self::Error> {
        match opcode {
            Opcode::Add => self.instructions.push(Instruction::Op(Op::Add)),
            Opcode::Sub => self.instructions.push(Instruction::Op(Op::Sub)),
            Opcode::Mul => self.instructions.push(Instruction::Op(Op::Mul)),
            Opcode::Div => self.instructions.push(Instruction::Op(Op::Div)),
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
            Instruction::Op(Op::Add),
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
        let input = "(3 + 4) * 3";
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
}
