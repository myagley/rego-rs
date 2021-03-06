use std::fmt;

use crate::value::Value;
use crate::vm::{BinOp, CollectType};

/// Intermediate representation of the stack machine
pub enum Ir {
    /// Push globals' reference to opstack
    LoadGlobal,
    /// Push immediate value to opstack
    LoadImmediate(Value<'static>),
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

    /// Push frame onto callstack and jump to label
    Call(String),
    /// Pop frame off callstack
    Return,
    /// Jump to label
    Jump(String),

    /// Pop value off of opstack, branch to calculated pc if undefined
    BranchUndefined(isize),
    /// Pop value off of opstack, branch to calculated pc if true
    BranchTrue(isize),
}

impl fmt::Display for Ir {
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
            Self::BranchTrue(offset) => write!(f, "btrue {}", offset),
        }
    }
}
