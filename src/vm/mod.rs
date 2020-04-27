use std::sync::Arc;

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
    Literal(Value<'a>),
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
    pub fn eval(&'a mut self) -> Result<Option<Value<'a>>, Error> {
        use Instruction::*;

        let mut pc = 0;
        self.value_stack.clear();

        while pc < self.instructions.len() {
            match self.instructions[pc] {
                Noop => (),
                Literal(ref v) => self.value_stack.push(Operand::from_ref(v)),
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
        Ok(self.value_stack.pop().and_then(|o| o.into_value()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eval() {
        let instructions = vec![
            Instruction::Literal(Value::Number(3.into())),
            Instruction::Literal(Value::Number(4.into())),
            Instruction::Op(Op::Add),
        ];

        let machine = Machine {
            instructions: Arc::new(instructions),
        };
        let mut instance = machine.instance();
        let result = instance.eval();
        println!("result: {:?}", result);
    }
}
