use crate::ast;
use crate::value::Value;

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

#[derive(Debug, Clone, PartialEq)]
pub enum Collection {
    Array(Vec<Expr>),
    Set(Vec<Expr>),
    Object(Vec<(Expr, Expr)>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Comprehension {
    Array(Box<Expr>, Box<Expr>),
    Set(Box<Expr>, Box<Expr>),
    Object(Box<(Expr, Expr)>, Box<Expr>),
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

pub trait Visitor {
    type Value;
    type Error;

    fn visit_expr(self, expr: Expr) -> Result<Self::Value, Self::Error>;

    fn visit_opcode(self, opcode: Opcode) -> Result<Self::Value, Self::Error>;

    fn visit_collection(self, target: Collection) -> Result<Self::Value, Self::Error>;

    fn visit_comprehension(self, target: Comprehension) -> Result<Self::Value, Self::Error>;
}

impl From<ast::Opcode> for Opcode {
    fn from(op: ast::Opcode) -> Self {
        match op {
            ast::Opcode::Add => Opcode::Add,
            ast::Opcode::Sub => Opcode::Sub,
            ast::Opcode::Mul => Opcode::Mul,
            ast::Opcode::Div => Opcode::Div,
            ast::Opcode::And => Opcode::And,
            ast::Opcode::Or => Opcode::Or,
            ast::Opcode::Lt => Opcode::Lt,
            ast::Opcode::Lte => Opcode::Lte,
            ast::Opcode::Gt => Opcode::Gt,
            ast::Opcode::Gte => Opcode::Gte,
            ast::Opcode::EqEq => Opcode::EqEq,
            ast::Opcode::Ne => Opcode::Ne,
            ast::Opcode::Eq => Opcode::Eq,
            ast::Opcode::Assign => Opcode::Assign,
        }
    }
}

impl From<ast::Term<'_>> for Expr {
    fn from(term: ast::Term<'_>) -> Self {
        match term {
            ast::Term::BinOp(left, op, right) => Expr::BinOp(
                Box::new(Expr::from(*left)),
                op.into(),
                Box::new(Expr::from(*right)),
            ),
            ast::Term::Scalar(value) => Expr::Scalar(value),
            ast::Term::Ref(r) => Expr::from(r),
        }
    }
}

impl From<ast::Ref<'_>> for Expr {
    fn from(r: ast::Ref<'_>) -> Self {
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

impl From<ast::RefTarget<'_>> for Expr {
    fn from(r: ast::RefTarget<'_>) -> Expr {
        match r {
            ast::RefTarget::Var(s) => Expr::Var(s.to_owned()),
            ast::RefTarget::Collection(collection) => Expr::from(collection),
            ast::RefTarget::ExprCall(call) => Expr::from(call),
            ast::RefTarget::ArrayCompr(compr) => Expr::from(compr),
            ast::RefTarget::SetCompr(compr) => Expr::from(compr),
            ast::RefTarget::ObjectCompr(compr) => Expr::from(compr),
        }
    }
}

impl From<ast::RefArg<'_>> for Expr {
    fn from(r: ast::RefArg<'_>) -> Expr {
        match r {
            ast::RefArg::Scalar(v) => Expr::Scalar(v),
            ast::RefArg::Var(s) => Expr::Var(s.to_owned()),
            ast::RefArg::Collection(collection) => Expr::from(collection),
            ast::RefArg::Anon => todo!(),
        }
    }
}

impl From<ast::Collection<'_>> for Expr {
    fn from(collection: ast::Collection<'_>) -> Self {
        let collection = match collection {
            ast::Collection::Array(items) => {
                Collection::Array(items.into_iter().map(Expr::from).collect())
            }
            ast::Collection::Set(items) => {
                Collection::Set(items.into_iter().map(Expr::from).collect())
            }
            ast::Collection::Object(items) => Collection::Object(
                items
                    .into_iter()
                    .map(|(k, v)| (Expr::from(k), Expr::from(v)))
                    .collect(),
            ),
        };
        Expr::Collection(collection)
    }
}

impl From<ast::ExprCall<'_>> for Expr {
    fn from(call: ast::ExprCall<'_>) -> Self {
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

impl From<ast::ArrayCompr<'_>> for Expr {
    fn from(compr: ast::ArrayCompr<'_>) -> Self {
        let (term, body) = compr.into_parts();
        Expr::Comprehension(Comprehension::Array(Box::new(Expr::from(term)), Box::new(Expr::from(body))))
    }
}

impl From<ast::SetCompr<'_>> for Expr {
    fn from(compr: ast::SetCompr<'_>) -> Self {
        let (term, body) = compr.into_parts();
        Expr::Comprehension(Comprehension::Set(Box::new(Expr::from(term)), Box::new(Expr::from(body))))
    }
}

impl From<ast::ObjectCompr<'_>> for Expr {
    fn from(compr: ast::ObjectCompr<'_>) -> Self {
        let ((key, value), body) = compr.into_parts();
        Expr::Comprehension(Comprehension::Object(Box::new((Expr::from(key), Expr::from(value))), Box::new(Expr::from(body))))
    }
}

impl From<ast::Query<'_>> for Expr {
    fn from(query: ast::Query<'_>) -> Self {
        query.into_statements().into_iter().fold(Expr::Scalar(Value::Bool(true)), |acc, s| {
            let (target, _with) = s.into_parts();
            let e = match target {
                ast::StatementTarget::Expr(t) => Expr::from(t),
                ast::StatementTarget::NotExpr(t) => Expr::Not(Box::new(Expr::from(t))),
                ast::StatementTarget::Some(v) => Expr::Some(v.into_iter().map(|s| s.to_owned()).collect()),
            };
            Expr::BinOp(Box::new(acc), Opcode::And, Box::new(e))
        })
    }
}
