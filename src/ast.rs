use crate::value::Value;

pub enum Term<'input> {
    BinOp(Box<Term<'input>>, Opcode, Box<Term<'input>>),
    Scalar(Value<'input>),
    Ref(Ref<'input>),
}

pub struct Ref<'input> {
    target: Box<RefTarget<'input>>,
    args: Vec<RefArg<'input>>,
}

impl<'input> Ref<'input> {
    pub fn new(target: Box<RefTarget<'input>>, args: Vec<RefArg<'input>>) -> Self {
        Self { target, args }
    }
}

pub enum RefTarget<'input> {
    Var(&'input str),
    Collection(Collection<'input>),
    ExprCall(ExprCall<'input>),
    ArrayCompr,
    SetCompr,
    ObjectCompr,
}

pub enum RefArg<'input> {
    Collection(Collection<'input>),
    Var(&'input str),
    Scalar(Value<'input>),
    Anon,
}

pub struct ExprCall<'input> {
    target: Ref<'input>,
    args: Vec<Box<Term<'input>>>,
}

impl<'input> ExprCall<'input> {
    pub fn new(target: Ref<'input>, args: Vec<Box<Term<'input>>>) -> Self {
        Self { target, args }
    }
}

pub enum Collection<'input> {
    Array(Vec<Box<Term<'input>>>),
    Set(Vec<Box<Term<'input>>>),
    Object(Vec<(Box<Term<'input>>, Box<Term<'input>>)>),
}

pub enum Opcode {
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
