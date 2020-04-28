use crate::value::Value;

pub trait Visitor<'input> {
    type Value;
    type Error;

    fn visit_term(self, term: &Term<'input>) -> Result<Self::Value, Self::Error>;

    fn visit_opcode(self, opcode: &Opcode) -> Result<Self::Value, Self::Error>;

    fn visit_ref(self, target: &Ref<'input>) -> Result<Self::Value, Self::Error>;

    fn visit_ref_target(self, target: &RefTarget<'input>) -> Result<Self::Value, Self::Error>;

    fn visit_ref_arg(self, target: &RefArg<'input>) -> Result<Self::Value, Self::Error>;

    fn visit_collection(self, target: &Collection<'input>) -> Result<Self::Value, Self::Error>;
}

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
    term: Term<'input>,
    as_term: Term<'input>,
}

impl<'input> With<'input> {
    pub fn new(term: Term<'input>, as_term: Term<'input>) -> Self {
        Self { term, as_term }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementTarget<'input> {
    Expr(Term<'input>),
    NotExpr(Term<'input>),
    Some(Vec<&'input str>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term<'input> {
    BinOp(Box<Term<'input>>, Opcode, Box<Term<'input>>),
    Scalar(Value<'input>),
    Ref(Ref<'input>),
}

impl<'input> Term<'input> {
    pub fn accept<V>(&self, visitor: V) -> Result<V::Value, V::Error>
    where
        V: Visitor<'input>,
    {
        visitor.visit_term(self)
    }
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

    pub fn target(&self) -> &RefTarget<'input> {
        &self.target
    }

    pub fn args(&self) -> &[RefArg<'input>] {
        &self.args
    }

    pub fn accept<V>(&self, visitor: V) -> Result<V::Value, V::Error>
    where
        V: Visitor<'input>,
    {
        visitor.visit_ref(self)
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

impl<'input> RefTarget<'input> {
    pub fn accept<V>(&self, visitor: V) -> Result<V::Value, V::Error>
    where
        V: Visitor<'input>,
    {
        visitor.visit_ref_target(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RefArg<'input> {
    Collection(Collection<'input>),
    Var(&'input str),
    Scalar(Value<'input>),
    Anon,
}

impl<'input> RefArg<'input> {
    pub fn accept<V>(&self, visitor: V) -> Result<V::Value, V::Error>
    where
        V: Visitor<'input>,
    {
        visitor.visit_ref_arg(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprCall<'input> {
    target: Ref<'input>,
    args: Vec<Term<'input>>,
}

impl<'input> ExprCall<'input> {
    pub fn new(target: Ref<'input>, args: Vec<Term<'input>>) -> Self {
        Self { target, args }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Collection<'input> {
    Array(Vec<Term<'input>>),
    Set(Vec<Term<'input>>),
    Object(Vec<(Term<'input>, Term<'input>)>),
}

impl<'input> Collection<'input> {
    pub fn accept<V>(&self, visitor: V) -> Result<V::Value, V::Error>
    where
        V: Visitor<'input>,
    {
        visitor.visit_collection(self)
    }
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

impl Opcode {
    pub fn accept<'input, V>(&self, visitor: V) -> Result<V::Value, V::Error>
    where
        V: Visitor<'input>,
    {
        visitor.visit_opcode(self)
    }
}
