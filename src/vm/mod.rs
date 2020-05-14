mod ir;
mod passes;
mod stack;

use std::collections::HashMap;
use std::sync::Arc;
use std::{fmt, mem};

use typed_arena::Arena;

use crate::ast::*;
use crate::value::{Map, Set, Value};

pub use ir::Ir;
pub use stack::Stack;

static UNDEFINED: Value = Value::Undefined;
static TRUE: Value = Value::Bool(true);
static FALSE: Value = Value::Bool(false);

macro_rules! static_bool {
    ( $expr:expr ) => {
        if $expr {
            &TRUE
        } else {
            &FALSE
        }
    };
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    /// Push globals' reference to opstack
    LoadGlobal,
    /// Push immediate value to opstack
    LoadImmediate(Value),
    /// Push local variable value reference to opstack
    LoadLocal(String),
    /// Pop value from opstack and store reference in locals
    StoreLocal(String),
    /// Pop immediate num of items from opstack, collect into CollectType, and push result to
    /// opstack
    Collect(CollectType, usize),
    /// Pop reference and target from opstack, index target by reference, push result to opstack
    Index,
    /// Duplicates the top of the stack
    Dup,
    /// Discard the top value on the stack
    Pop,

    /// Pop two operands from opstack, apply binop, and push result to opstack
    BinOp(BinOp),

    /// Push frame onto callstack and jump to immediate instruction
    Call(usize),
    /// Pop frame off callstack
    Return,
    /// Jump to immediate instruction
    Jump(usize),

    /// Pop value off of opstack, branch to calculated pc if undefined
    BranchUndefined(isize),
    /// Pop value off of opstack, branch to calculated pc if defined
    BranchDefined(isize),
    /// Pop value off of opstack, branch to calculated pc if true
    BranchTrue(isize),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::LoadGlobal => write!(f, "loadg"),
            Self::LoadImmediate(v) => write!(f, "loadi {}", v),
            Self::LoadLocal(v) => write!(f, "loadl {}", v),
            Self::StoreLocal(v) => write!(f, "storel {}", v),
            Self::Collect(ty, size) => write!(f, "collect {} {}", ty, size),
            Self::Index => write!(f, "index"),
            Self::Dup => write!(f, "dup"),
            Self::Pop => write!(f, "pop"),
            Self::BinOp(op) => write!(f, "binop {}", op),
            Self::Call(pc) => write!(f, "call {}", pc),
            Self::Return => write!(f, "ret"),
            Self::Jump(pc) => write!(f, "jump {}", pc),
            Self::BranchUndefined(offset) => write!(f, "bundef {}", offset),
            Self::BranchDefined(offset) => write!(f, "bdef {}", offset),
            Self::BranchTrue(offset) => write!(f, "btrue {}", offset),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Lt,
    Lte,
    Gt,
    Gte,
    EqEq,
    Ne,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "add"),
            BinOp::Sub => write!(f, "sub"),
            BinOp::Mul => write!(f, "mul"),
            BinOp::Div => write!(f, "div"),
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
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
struct Frame<'a> {
    pc: usize,
    locals: HashMap<&'a str, &'a Value>,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidNumArgs(usize, usize),
    InvalidValueType(&'static str),
    StackUnderflow,
    StackOverflow,
    AddressUnderflow,
    UnknownReference(String),
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
            Error::AddressUnderflow => write!(f, "pc is negative after applying branch offset"),
            Error::UnknownReference(label) => write!(f, "unknown reference: {}", label),
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
    pub fn from_query(query: Expr) -> Result<Self, Error> {
        Self::compile(query, vec![])
    }

    pub fn compile(mut query: Expr, mut modules: Vec<Module>) -> Result<Self, Error> {
        let mut codegen = passes::Codegen::new();
        codegen.push_ir(Ir::Jump("$__query".to_string()));

        for module in &mut modules {
            // Input resolution
            let mut input = passes::InputResolver::new();
            module.accept(&mut input)?;

            // Rule resolution
            let mut rules = passes::RuleResolver::new(module.package());
            module.accept(&mut rules)?;

            // Const Eval
            module.accept(&mut passes::ConstEval)?;

            // Codegen
            module.accept(&mut codegen)?;
        }

        // Compile the query
        // Input resolution
        let mut input = passes::InputResolver::new();
        query.accept(&mut input)?;

        // rule resolution
        let mut rules = passes::RuleResolver::new(&vec![]);
        query.accept(&mut rules)?;
        query.accept(&mut passes::ConstEval)?;
        codegen.push_label("$__query");
        query.accept(&mut codegen)?;
        let query = CompiledQuery {
            instructions: Arc::new(codegen.assemble()?),
        };
        Ok(query)
    }

    pub fn eval(&self, input: Value) -> Result<Value, Error> {
        let mut instance = Instance {
            instructions: self.instructions.clone(),
            opstack: Stack::new(),
            callstack: Stack::new(),
            heap: Arena::new(),
            locals: HashMap::new(),
            input,
        };
        instance.eval()
    }
}

impl fmt::Display for CompiledQuery {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, instruction) in self.instructions.iter().enumerate() {
            writeln!(f, "{:>5}: {}", i, instruction)?;
        }
        Ok(())
    }
}

struct Instance<'m> {
    instructions: Arc<Vec<Instruction>>,
    opstack: Stack<&'m Value>,
    callstack: Stack<Frame<'m>>,
    heap: Arena<Value>,
    locals: HashMap<&'m str, &'m Value>,
    input: Value,
}

impl<'a> Instance<'a> {
    fn eval(&'a mut self) -> Result<Value, Error> {
        use Instruction::*;

        let mut pc = 0;
        self.opstack.clear();

        while pc < self.instructions.len() {
            println!("{:>5}: {}", pc, self.instructions[pc]);

            match self.instructions[pc] {
                LoadGlobal => self.opstack.push(&self.input)?,
                LoadImmediate(ref v) => self.opstack.push(v)?,
                LoadLocal(ref v) => {
                    if let Some(value) = self.locals.get(&v.as_str()) {
                        self.opstack.push(*value)?;
                    } else {
                        self.opstack.push(&UNDEFINED)?;
                    }
                }
                StoreLocal(ref v) => {
                    let value = self.opstack.pop()?;
                    if self.locals.contains_key(v.as_str()) {
                        self.opstack.push(&UNDEFINED)?;
                    } else {
                        self.locals.insert(v, value);
                        self.opstack.push(&TRUE)?;
                    }
                }
                Collect(ref ty, len) => match ty {
                    CollectType::Array => {
                        let mut result = vec![];
                        for _i in 0..len {
                            let value = self.opstack.pop()?;
                            result.push(value.clone());
                        }
                        let result = self.heap.alloc(Value::Array(result));
                        self.opstack.push(&*result)?;
                    }
                    CollectType::Set => {
                        let mut result = Set::new();
                        for _i in 0..len {
                            let value = self.opstack.pop()?;
                            result.insert(value.clone());
                        }
                        let result = self.heap.alloc(Value::Set(result));
                        self.opstack.push(&*result)?;
                    }
                    CollectType::Map => {
                        let mut result = Map::new();
                        for _i in 0..len {
                            let value = self.opstack.pop()?;
                            let key = self.opstack.pop()?;
                            result.insert(key.clone(), value.clone());
                        }
                        let result = self.heap.alloc(Value::Object(result));
                        self.opstack.push(&*result)?;
                    }
                },
                Index => {
                    let arg = self.opstack.pop()?;
                    let target = self.opstack.pop()?;
                    let result = target.get(arg).unwrap_or_else(|| &UNDEFINED);
                    self.opstack.push(result)?;
                }
                Dup => {
                    let value = self.opstack.pop()?;
                    self.opstack.push(value)?;
                    self.opstack.push(value)?;
                }
                Pop => {
                    self.opstack.pop()?;
                }
                BinOp(ref binop) => {
                    let right = self.opstack.pop()?;
                    let left = self.opstack.pop()?;
                    let result = match binop {
                        self::BinOp::Add => self.heap.alloc(left + right),
                        self::BinOp::Sub => self.heap.alloc(left - right),
                        self::BinOp::Mul => self.heap.alloc(left * right),
                        self::BinOp::Div => self.heap.alloc(left / right),
                        self::BinOp::And => {
                            if left.is_boolean() && right.is_boolean() {
                                static_bool!(left.is_true() && right.is_true())
                            } else {
                                &UNDEFINED
                            }
                        }
                        self::BinOp::Or => {
                            if left.is_boolean() && right.is_boolean() {
                                static_bool!(left.is_true() || right.is_true())
                            } else {
                                &UNDEFINED
                            }
                        }
                        self::BinOp::Lt => static_bool!(left.lt(&right)),
                        self::BinOp::Lte => static_bool!(left.le(&right)),
                        self::BinOp::Gt => static_bool!(left.gt(&right)),
                        self::BinOp::Gte => static_bool!(left.ge(&right)),
                        self::BinOp::EqEq => static_bool!(left.eq(&right)),
                        self::BinOp::Ne => static_bool!(left.ne(&right)),
                    };
                    self.opstack.push(result)?;
                }
                Call(jpc) => {
                    let prev = mem::replace(&mut self.locals, HashMap::new());
                    let frame = Frame { locals: prev, pc };
                    self.callstack.push(frame)?;
                    pc = jpc;
                    continue;
                }
                Return => {
                    let frame = self.callstack.pop()?;
                    self.locals = frame.locals;
                    pc = frame.pc;
                }
                Jump(jpc) => {
                    pc = jpc;
                    continue;
                }
                BranchDefined(offset) => {
                    let value = self.opstack.pop()?;
                    if !value.is_undefined() {
                        let next = pc as isize + offset;
                        if next < 0 {
                            return Err(Error::AddressUnderflow);
                        } else {
                            pc = next as usize;
                            continue;
                        }
                    }
                }
                BranchUndefined(offset) => {
                    let value = self.opstack.pop()?;
                    if value.is_undefined() {
                        let next = pc as isize + offset;
                        if next < 0 {
                            return Err(Error::AddressUnderflow);
                        } else {
                            pc = next as usize;
                            continue;
                        }
                    }
                }
                BranchTrue(offset) => {
                    let value = self.opstack.pop()?;
                    if value.is_true() {
                        let next = pc as isize + offset;
                        if next < 0 {
                            return Err(Error::AddressUnderflow);
                        } else {
                            pc = next as usize;
                            continue;
                        }
                    }
                }
            }
            pc += 1
        }
        let result = self.opstack.pop().map(|o| o.clone());
        debug_assert!(self.opstack.len() == 0);
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::convert::TryFrom;

    use crate::parser::{parse_module, parse_query};

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
        println!("{}", query);
        let result = query.eval(Value::Undefined);
        println!("result: {:?}", result);
    }

    #[test]
    fn test_function_call() {
        let instructions = vec![
            Instruction::Jump(5),
            Instruction::LoadImmediate(Value::Number(3.into())),
            Instruction::LoadImmediate(Value::Number(4.into())),
            Instruction::BinOp(BinOp::Add),
            Instruction::Return,
            Instruction::Call(1),
        ];

        let query = CompiledQuery {
            instructions: Arc::new(instructions),
        };
        println!("{}", query);
        let result = query.eval(Value::Undefined).unwrap();
        assert_eq!(Value::Number(7.into()), result);
        println!("result: {:?}", result);
    }

    #[test]
    fn test_branch_undefined() {
        let instructions = vec![
            Instruction::Jump(10),
            Instruction::LoadImmediate(Value::Number(3.into())),
            Instruction::LoadImmediate(Value::Null),
            Instruction::BinOp(BinOp::Add),
            Instruction::Dup,
            Instruction::BranchUndefined(2),
            Instruction::Return,
            Instruction::Pop,
            Instruction::LoadImmediate(Value::Number(3.into())),
            Instruction::Return,
            Instruction::Call(1),
        ];

        let query = CompiledQuery {
            instructions: Arc::new(instructions),
        };
        println!("{}", query);
        let result = query.eval(Value::Undefined).unwrap();
        assert_eq!(Value::Number(3.into()), result);
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

    #[test]
    fn test_default_rule() {
        let module = r###"
            package opa.test

            default allow = true
        "###;
        let module = parse_module(&module).unwrap();
        let module = Module::try_from(module).unwrap();

        let query = "data.opa.test.allow";
        let query = parse_query(&query).unwrap();
        let query = Expr::try_from(query).unwrap();

        let query = CompiledQuery::compile(query, vec![module]).unwrap();
        println!("{}", query);
        let result = query.eval(Value::Null).unwrap();
        let expected = Value::Bool(true);
        assert_eq!(expected, result);
    }

    #[test]
    fn test_complete_rule() {
        let module = r###"
            package opa.test

            a := "hello"
        "###;
        let module = parse_module(&module).unwrap();
        let module = Module::try_from(module).unwrap();

        let query = "data.opa.test.a";
        let query = parse_query(&query).unwrap();
        let query = Expr::try_from(query).unwrap();

        let query = CompiledQuery::compile(query, vec![module]).unwrap();
        println!("{}", query);
        let result = query.eval(Value::Null).unwrap();
        let expected = Value::String("hello".to_string());
        assert_eq!(expected, result);
    }

    #[test]
    fn test_complete_rule_multiple_clauses() {
        let module = r###"
            package opa.test

            a := "hello" { 3 == 2 }
            a := "hello"
        "###;
        let module = parse_module(&module).unwrap();
        let module = Module::try_from(module).unwrap();

        let query = "data.opa.test.a";
        let query = parse_query(&query).unwrap();
        let query = Expr::try_from(query).unwrap();

        let query = CompiledQuery::compile(query, vec![module]).unwrap();
        println!("{}", query);
        let result = query.eval(Value::Null).unwrap();
        let expected = Value::String("hello".to_string());
        assert_eq!(expected, result);
    }

    #[test]
    fn test_complete_rule_single_clause_default() {
        let module = r###"
            package opa.test

            default a = "world"

            a := "hello" { false }
        "###;
        let module = parse_module(&module).unwrap();
        let module = Module::try_from(module).unwrap();

        let query = "data.opa.test.a";
        let query = parse_query(&query).unwrap();
        let query = Expr::try_from(query).unwrap();

        let query = CompiledQuery::compile(query, vec![module]).unwrap();
        println!("{}", query);
        let result = query.eval(Value::Null).unwrap();
        let expected = Value::String("world".to_string());
        assert_eq!(expected, result);
    }

    #[test]
    fn test_complete_rule_multiple_clauses_default() {
        let module = r###"
            package opa.test

            default a = "world"

            a := "hello" { false }
            a := "hello" { false }
        "###;
        let module = parse_module(&module).unwrap();
        let module = Module::try_from(module).unwrap();

        let query = "data.opa.test.a";
        let query = parse_query(&query).unwrap();
        let query = Expr::try_from(query).unwrap();

        let query = CompiledQuery::compile(query, vec![module]).unwrap();
        println!("{}", query);
        let result = query.eval(Value::Null).unwrap();
        let expected = Value::String("world".to_string());
        assert_eq!(expected, result);
    }

    #[test]
    fn test_complete_rule_multiple_rules() {
        let module = r###"
            package opa.test

            default a = "world"

            b := 3
            a := "hello" {
                b == 3
            }
        "###;
        let module = parse_module(&module).unwrap();
        let module = Module::try_from(module).unwrap();

        let query = "data.opa.test.a";
        let query = parse_query(&query).unwrap();
        let query = Expr::try_from(query).unwrap();

        let query = CompiledQuery::compile(query, vec![module]).unwrap();
        println!("{}", query);
        let result = query.eval(Value::Null).unwrap();
        let expected = Value::String("hello".to_string());
        assert_eq!(expected, result);
    }

    #[test]
    fn test_complete_rule_input() {
        let module = r###"
            package opa.test

            default a = "world"

            a := "hello" {
                input == 3
            }
        "###;
        let module = parse_module(&module).unwrap();
        let module = Module::try_from(module).unwrap();

        let query = "data.opa.test.a";
        let query = parse_query(&query).unwrap();
        let query = Expr::try_from(query).unwrap();

        let query = CompiledQuery::compile(query, vec![module]).unwrap();
        println!("{}", query);
        let result = query.eval(Value::Number(3.into())).unwrap();
        let expected = Value::String("hello".to_string());
        assert_eq!(expected, result);

        let result = query.eval(Value::Number(2.into())).unwrap();
        let expected = Value::String("world".to_string());
        assert_eq!(expected, result);
    }

    #[test]
    fn test_complete_rule_multiple_statements() {
        let module = r###"
            package opa.test

            default a = "world"

            a := "hello" {
                input >= 2
                input >= 4
            }
        "###;
        let module = parse_module(&module).unwrap();
        let module = Module::try_from(module).unwrap();

        let query = "data.opa.test.a";
        let query = parse_query(&query).unwrap();
        let query = Expr::try_from(query).unwrap();

        let query = CompiledQuery::compile(query, vec![module]).unwrap();
        println!("{}", query);
        let result = query.eval(Value::Number(4.into())).unwrap();
        let expected = Value::String("hello".to_string());
        assert_eq!(expected, result);

        let result = query.eval(Value::Number(3.into())).unwrap();
        let expected = Value::String("world".to_string());
        assert_eq!(expected, result);
    }

    #[test]
    fn test_local_var() {
        let module = r###"
            package opa.test

            default a = "world"

            a := "hello" {
                c := input
                c >= 2
                c >= 4
            }
        "###;
        let module = parse_module(&module).unwrap();
        let module = Module::try_from(module).unwrap();

        let query = "data.opa.test.a";
        let query = parse_query(&query).unwrap();
        let query = Expr::try_from(query).unwrap();

        let query = CompiledQuery::compile(query, vec![module]).unwrap();
        println!("{}", query);
        let result = query.eval(Value::Number(4.into())).unwrap();
        let expected = Value::String("hello".to_string());
        assert_eq!(expected, result);

        let result = query.eval(Value::Number(3.into())).unwrap();
        let expected = Value::String("world".to_string());
        assert_eq!(expected, result);
    }

    #[test]
    fn test_complete_elses() {
        let module = r###"
            package opa.test

            default a = "world"

            a = "hello" {
                input == 3
            } else = "there" {
                input == 4
            } else = "ok" {
                input == 5
            } else = "something" {
                input == 5
            }
        "###;
        let module = parse_module(&module).unwrap();
        let module = Module::try_from(module).unwrap();

        let query = "data.opa.test.a";
        let query = parse_query(&query).unwrap();
        let query = Expr::try_from(query).unwrap();

        let query = CompiledQuery::compile(query, vec![module]).unwrap();
        println!("{}", query);
        let result = query.eval(Value::Number(3.into())).unwrap();
        let expected = Value::String("hello".to_string());
        assert_eq!(expected, result);

        let result = query.eval(Value::Number(4.into())).unwrap();
        let expected = Value::String("there".to_string());
        assert_eq!(expected, result);

        let result = query.eval(Value::Number(5.into())).unwrap();
        let expected = Value::String("ok".to_string());
        assert_eq!(expected, result);

        let result = query.eval(Value::Number(6.into())).unwrap();
        let expected = Value::String("world".to_string());
        assert_eq!(expected, result);
    }
}
