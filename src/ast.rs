use crate::parser::tree;
use crate::value::Value;

pub trait Visitor {
    type Value;
    type Error;

    fn visit_expr(self, expr: &Expr) -> Result<Self::Value, Self::Error>;

    fn visit_opcode(self, opcode: &Opcode) -> Result<Self::Value, Self::Error>;

    fn visit_collection(self, target: &Collection) -> Result<Self::Value, Self::Error>;

    fn visit_comprehension(self, target: &Comprehension) -> Result<Self::Value, Self::Error>;
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
    Union,
    Intersect,
}

impl Opcode {
    pub fn accept<V>(&self, visitor: V) -> Result<V::Value, V::Error>
    where
        V: Visitor,
    {
        visitor.visit_opcode(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Collection {
    Array(Vec<Expr>),
    Set(Vec<Expr>),
    Object(Vec<(Expr, Expr)>),
}

impl Collection {
    pub fn accept<V>(&self, visitor: V) -> Result<V::Value, V::Error>
    where
        V: Visitor,
    {
        visitor.visit_collection(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Comprehension {
    Array(Box<Expr>, Box<Expr>),
    Set(Box<Expr>, Box<Expr>),
    Object(Box<(Expr, Expr)>, Box<Expr>),
}

impl Comprehension {
    pub fn accept<V>(&self, visitor: V) -> Result<V::Value, V::Error>
    where
        V: Visitor,
    {
        visitor.visit_comprehension(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Scalar(Value),
    Collection(Collection),
    Comprehension(Comprehension),
    Var(String),

    BinOp(Box<Expr>, Opcode, Box<Expr>),
    Index(Vec<Expr>),
    Call(String, Vec<Expr>),

    Not(Box<Expr>),
    Some(Vec<String>),
}

impl Expr {
    pub fn accept<V>(&self, visitor: V) -> Result<V::Value, V::Error>
    where
        V: Visitor,
    {
        visitor.visit_expr(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Clause {
    value: Expr,
    body: Expr,
    is_else: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Rule {
    name: String,
    default: Option<Expr>,
    value: Vec<Clause>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    name: String,
    rules: Vec<Rule>,
}

impl From<tree::Opcode> for Opcode {
    fn from(op: tree::Opcode) -> Self {
        match op {
            tree::Opcode::Add => Opcode::Add,
            tree::Opcode::Sub => Opcode::Sub,
            tree::Opcode::Mul => Opcode::Mul,
            tree::Opcode::Div => Opcode::Div,
            tree::Opcode::And => Opcode::And,
            tree::Opcode::Or => Opcode::Or,
            tree::Opcode::Lt => Opcode::Lt,
            tree::Opcode::Lte => Opcode::Lte,
            tree::Opcode::Gt => Opcode::Gt,
            tree::Opcode::Gte => Opcode::Gte,
            tree::Opcode::EqEq => Opcode::EqEq,
            tree::Opcode::Ne => Opcode::Ne,
            tree::Opcode::Eq => Opcode::Eq,
            tree::Opcode::Assign => Opcode::Assign,
        }
    }
}

impl From<tree::Term<'_>> for Expr {
    fn from(term: tree::Term<'_>) -> Self {
        match term {
            tree::Term::BinOp(left, op, right) => Expr::BinOp(
                Box::new(Expr::from(*left)),
                op.into(),
                Box::new(Expr::from(*right)),
            ),
            tree::Term::Scalar(value) => Expr::Scalar(value),
            tree::Term::Ref(r) => Expr::from(r),
        }
    }
}

impl From<tree::Ref<'_>> for Expr {
    fn from(r: tree::Ref<'_>) -> Self {
        let (target, args) = r.into_parts();
        if args.len() == 0 {
            Expr::from(*target)
        } else {
            let len = args.len() + 1;
            let target = Expr::from(*target);
            let mut args = args.into_iter().map(Expr::from).collect();
            let mut indexed = Vec::with_capacity(len);
            indexed.push(target);
            indexed.append(&mut args);
            Expr::Index(indexed)
        }
    }
}

impl From<tree::RefTarget<'_>> for Expr {
    fn from(r: tree::RefTarget<'_>) -> Expr {
        match r {
            tree::RefTarget::Var(s) => Expr::Var(s.to_owned()),
            tree::RefTarget::Collection(collection) => Expr::from(collection),
            tree::RefTarget::ExprCall(call) => Expr::from(call),
            tree::RefTarget::ArrayCompr(compr) => Expr::from(compr),
            tree::RefTarget::SetCompr(compr) => Expr::from(compr),
            tree::RefTarget::ObjectCompr(compr) => Expr::from(compr),
        }
    }
}

impl From<tree::RefArg<'_>> for Expr {
    fn from(r: tree::RefArg<'_>) -> Expr {
        match r {
            tree::RefArg::Scalar(v) => Expr::Scalar(v),
            tree::RefArg::Var(s) => Expr::Var(s.to_owned()),
            tree::RefArg::Collection(collection) => Expr::from(collection),
            tree::RefArg::Anon => todo!(),
        }
    }
}

impl From<tree::Collection<'_>> for Expr {
    fn from(collection: tree::Collection<'_>) -> Self {
        let collection = match collection {
            tree::Collection::Array(items) => {
                Collection::Array(items.into_iter().map(Expr::from).collect())
            }
            tree::Collection::Set(items) => {
                Collection::Set(items.into_iter().map(Expr::from).collect())
            }
            tree::Collection::Object(items) => Collection::Object(
                items
                    .into_iter()
                    .map(|(k, v)| (Expr::from(k), Expr::from(v)))
                    .collect(),
            ),
        };
        Expr::Collection(collection)
    }
}

impl From<tree::ExprCall<'_>> for Expr {
    fn from(call: tree::ExprCall<'_>) -> Self {
        let (target, args) = call.into_parts();
        let target = Expr::from(target);
        let mut args = args.into_iter().map(Expr::from).collect();
        let mut items = Vec::new();
        items.push(target);
        items.append(&mut args);
        // TODO fix this
        Expr::Call("hello".to_string(), items)
    }
}

impl From<tree::ArrayCompr<'_>> for Expr {
    fn from(compr: tree::ArrayCompr<'_>) -> Self {
        let (term, body) = compr.into_parts();
        Expr::Comprehension(Comprehension::Array(
            Box::new(Expr::from(term)),
            Box::new(Expr::from(body)),
        ))
    }
}

impl From<tree::SetCompr<'_>> for Expr {
    fn from(compr: tree::SetCompr<'_>) -> Self {
        let (term, body) = compr.into_parts();
        Expr::Comprehension(Comprehension::Set(
            Box::new(Expr::from(term)),
            Box::new(Expr::from(body)),
        ))
    }
}

impl From<tree::ObjectCompr<'_>> for Expr {
    fn from(compr: tree::ObjectCompr<'_>) -> Self {
        let ((key, value), body) = compr.into_parts();
        Expr::Comprehension(Comprehension::Object(
            Box::new((Expr::from(key), Expr::from(value))),
            Box::new(Expr::from(body)),
        ))
    }
}

impl From<tree::Query<'_>> for Expr {
    fn from(query: tree::Query<'_>) -> Self {
        query
            .into_statements()
            .into_iter()
            .fold(Expr::Scalar(Value::Bool(true)), |acc, s| {
                let (target, _with) = s.into_parts();
                let e = match target {
                    tree::StatementTarget::Expr(t) => Expr::from(t),
                    tree::StatementTarget::NotExpr(t) => Expr::Not(Box::new(Expr::from(t))),
                    tree::StatementTarget::Some(v) => {
                        Expr::Some(v.into_iter().map(|s| s.to_owned()).collect())
                    }
                };
                Expr::BinOp(Box::new(acc), Opcode::And, Box::new(e))
            })
    }
}
