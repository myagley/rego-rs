use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct Query<'input> {
    statements: Vec<Statement<'input>>,
}

impl<'input> Query<'input> {
    pub fn new(statements: Vec<Statement<'input>>) -> Self {
        Self { statements }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement<'input> {
    target: StatementTarget<'input>,
    with: Vec<With<'input>>,
}

impl<'input> Statement<'input> {
    pub fn new(target: StatementTarget<'input>, with: Vec<With<'input>>) -> Self {
        Self { target, with }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct With<'input> {
    term: Box<Term<'input>>,
    as_term: Box<Term<'input>>,
}

impl<'input> With<'input> {
    pub fn new(term: Box<Term<'input>>, as_term: Box<Term<'input>>) -> Self {
        Self { term, as_term }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementTarget<'input> {
    Expr(Box<Term<'input>>),
    NotExpr(Box<Term<'input>>),
    Some(Vec<&'input str>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term<'input> {
    BinOp(Box<Term<'input>>, Opcode, Box<Term<'input>>),
    Scalar(Value<'input>),
    Ref(Ref<'input>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ref<'input> {
    target: Box<RefTarget<'input>>,
    args: Vec<RefArg<'input>>,
}

impl<'input> Ref<'input> {
    pub fn new(target: Box<RefTarget<'input>>, args: Vec<RefArg<'input>>) -> Self {
        Self { target, args }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RefTarget<'input> {
    Var(&'input str),
    Collection(Collection<'input>),
    ExprCall(ExprCall<'input>),
    ArrayCompr,
    SetCompr,
    ObjectCompr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RefArg<'input> {
    Collection(Collection<'input>),
    Var(&'input str),
    Scalar(Value<'input>),
    Anon,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprCall<'input> {
    target: Ref<'input>,
    args: Vec<Box<Term<'input>>>,
}

impl<'input> ExprCall<'input> {
    pub fn new(target: Ref<'input>, args: Vec<Box<Term<'input>>>) -> Self {
        Self { target, args }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Collection<'input> {
    Array(Vec<Box<Term<'input>>>),
    Set(Vec<Box<Term<'input>>>),
    Object(Vec<(Box<Term<'input>>, Box<Term<'input>>)>),
}

#[derive(Debug, Clone, PartialEq)]
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
    Eq,
    Assign,
}
