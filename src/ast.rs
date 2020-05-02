use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;

use crate::parser::tree;
use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    ConflictingRules(String),
    MultipleDefaults(String),
    Unsupported(&'static str),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::ConflictingRules(s) => write!(f, "conflicting rules found for {}", s),
            Error::MultipleDefaults(s) => write!(f, "multiple default rules named {} found", s),
            Error::Unsupported(s) => write!(f, "{} are unsupported", s),
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

pub trait Visitor {
    type Value;
    type Error;

    fn visit_expr(self, expr: Expr) -> Result<Self::Value, Self::Error>;

    fn visit_collection(self, collection: Collection) -> Result<Self::Value, Self::Error>;

    fn visit_comprehension(self, _comprehension: Comprehension)
        -> Result<Self::Value, Self::Error>;
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

#[derive(Debug, Clone, PartialEq)]
pub enum Collection {
    Array(Vec<Expr>),
    Set(Vec<Expr>),
    Object(Vec<(Expr, Expr)>),
}

impl Collection {
    pub fn accept<V>(self, visitor: V) -> Result<V::Value, V::Error>
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
    pub fn accept<V>(self, visitor: V) -> Result<V::Value, V::Error>
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
    pub fn accept<V>(self, visitor: V) -> Result<V::Value, V::Error>
    where
        V: Visitor,
    {
        visitor.visit_expr(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Clause {
    body: Expr,
    value: Expr,
    is_else: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Rule {
    name: String,
    default: Option<Expr>,
    head: RuleHead,
    clauses: Vec<Clause>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RuleHead {
    None,
    Complete(Expr),
    Set(Expr),
    Object(Expr, Expr),
    Function(Vec<Expr>, Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    package: Vec<String>,
    rules: Vec<Rule>,
}

impl<'a> TryFrom<tree::Module<'a>> for Module {
    type Error = Error;

    fn try_from(module: tree::Module<'a>) -> Result<Self, Self::Error> {
        let (package, _imports, rules) = module.into_parts();
        let package = package.iter().map(|s| s.to_string()).collect();

        let rules: Vec<Rule> = rules
            .into_iter()
            .fold(HashMap::new(), |mut acc, r| {
                acc.entry(r.name()).or_insert_with(|| Vec::new()).push(r);
                acc
            })
            .iter()
            .map(|(name, rules)| {
                let default = extract_default(&rules)?
                    .map(|default| Expr::try_from(default.into_term()))
                    .transpose()?;

                let mut rules = rules
                    .into_iter()
                    .filter(|r| !matches!(r, tree::Rule::Default(_)));
                let rule = if let Some(first) = rules.next() {
                    let mut clauses = Vec::new();
                    let (head, mut next_clauses) =
                        <(RuleHead, Vec<Clause>)>::try_from(first.clone())?;
                    clauses.append(&mut next_clauses);

                    for rule in rules.into_iter() {
                        let (next_head, mut next_clauses) =
                            <(RuleHead, Vec<Clause>)>::try_from(rule.clone())?;
                        if is_conflicting(&next_head, &head) {
                            return Err(Error::ConflictingRules(name.to_string()));
                        }
                        clauses.append(&mut next_clauses);
                    }
                    Rule {
                        name: name.to_string(),
                        default,
                        head,
                        clauses,
                    }
                } else {
                    Rule {
                        name: name.to_string(),
                        default,
                        head: RuleHead::None,
                        clauses: Vec::new(),
                    }
                };
                Ok(rule)
            })
            .collect::<Result<Vec<Rule>, Self::Error>>()?;

        let module = Module { package, rules };
        Ok(module)
    }
}

impl TryFrom<tree::Rule<'_>> for (RuleHead, Vec<Clause>) {
    type Error = Error;

    fn try_from(rule: tree::Rule<'_>) -> Result<Self, Self::Error> {
        let result = match rule {
            tree::Rule::Complete(complete) => {
                let (value, body) = complete.into_parts();
                let expr = value
                    .map(Expr::try_from)
                    .unwrap_or_else(|| Ok(Expr::Scalar(Value::Bool(true))))?;
                let clauses = body
                    .map(Vec::<Clause>::try_from)
                    .transpose()?
                    .unwrap_or_else(|| Vec::new());
                (RuleHead::Complete(expr), clauses)
            }
            tree::Rule::Set(set) => {
                let (key, body) = set.into_parts();
                let expr = Expr::try_from(key)?;
                let clauses = body
                    .map(Vec::<Clause>::try_from)
                    .transpose()?
                    .unwrap_or_else(|| Vec::new());
                (RuleHead::Set(expr), clauses)
            }
            tree::Rule::Object(obj) => {
                let (key, value, body) = obj.into_parts();
                let key = Expr::try_from(key)?;
                let value = Expr::try_from(value)?;
                let clauses = body
                    .map(Vec::<Clause>::try_from)
                    .transpose()?
                    .unwrap_or_else(|| Vec::new());
                (RuleHead::Object(key, value), clauses)
            }
            tree::Rule::Function(function) => {
                let (args, value, body) = function.into_parts();
                let args = args
                    .into_iter()
                    .map(Expr::try_from)
                    .collect::<Result<Vec<Expr>, Error>>()?;
                let value = Expr::try_from(value)?;
                let clauses = body
                    .map(Vec::<Clause>::try_from)
                    .transpose()?
                    .unwrap_or_else(|| Vec::new());
                (RuleHead::Function(args, value), clauses)
            }
            _ => (RuleHead::None, Vec::new()),
        };
        Ok(result)
    }
}

impl TryFrom<tree::RuleBody<'_>> for Vec<Clause> {
    type Error = Error;

    fn try_from(body: tree::RuleBody<'_>) -> Result<Self, Self::Error> {
        let (query, tail) = body.into_parts();
        let body = Expr::try_from(query)?;
        let clause = Clause {
            body,
            value: Expr::Scalar(Value::Bool(true)),
            is_else: false,
        };
        let mut clauses = vec![clause];
        let mut tail = tail
            .into_iter()
            .map(Clause::try_from)
            .collect::<Result<Vec<Clause>, Error>>()?;
        clauses.append(&mut tail);
        Ok(clauses)
    }
}

impl TryFrom<tree::RuleBodyTail<'_>> for Clause {
    type Error = Error;

    fn try_from(body: tree::RuleBodyTail<'_>) -> Result<Self, Self::Error> {
        let (maybe_else, query) = body.into_parts();
        let (is_else, value) = maybe_else
            .and_then(|e| e.into_term().map(|t| Ok((true, Expr::try_from(t)?))))
            .transpose()?
            .unwrap_or_else(|| (false, Expr::Scalar(Value::Bool(true))));
        let body = Expr::try_from(query)?;
        let clause = Clause {
            body,
            value,
            is_else,
        };
        Ok(clause)
    }
}

fn extract_default<'a>(
    rules: &Vec<tree::Rule<'a>>,
) -> Result<Option<tree::DefaultRule<'a>>, Error> {
    let default = rules
        .iter()
        .filter_map(|r| match r {
            tree::Rule::Default(d) => Some(d.clone()),
            _ => None,
        })
        .next();
    Ok(default)
}

fn is_conflicting(left: &RuleHead, right: &RuleHead) -> bool {
    match (left, right) {
        (RuleHead::Complete(a), RuleHead::Complete(b)) if a == b => false,
        (RuleHead::Set(_), RuleHead::Set(_)) => false,
        (RuleHead::Object(_, _), RuleHead::Object(_, _)) => false,
        (RuleHead::Function(_, _), RuleHead::Function(_, _)) => false,
        _ => true,
    }
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

impl TryFrom<tree::Term<'_>> for Expr {
    type Error = Error;

    fn try_from(term: tree::Term<'_>) -> Result<Self, Self::Error> {
        let expr = match term {
            tree::Term::BinOp(left, op, right) => Expr::BinOp(
                Box::new(Expr::try_from(*left)?),
                op.into(),
                Box::new(Expr::try_from(*right)?),
            ),
            tree::Term::Scalar(value) => Expr::Scalar(value),
            tree::Term::Ref(r) => Expr::try_from(r)?,
        };
        Ok(expr)
    }
}

impl TryFrom<tree::Ref<'_>> for Expr {
    type Error = Error;

    fn try_from(r: tree::Ref<'_>) -> Result<Self, Self::Error> {
        let (target, args) = r.into_parts();
        if args.len() == 0 {
            Expr::try_from(*target)
        } else {
            let len = args.len() + 1;
            let target = Expr::try_from(*target)?;

            let mut new_args = Vec::with_capacity(len);
            for arg in args {
                new_args.push(Expr::try_from(arg)?);
            }

            let mut indexed = Vec::with_capacity(len);
            indexed.push(target);
            indexed.append(&mut new_args);
            Ok(Expr::Index(indexed))
        }
    }
}

impl TryFrom<tree::RefTarget<'_>> for Expr {
    type Error = Error;

    fn try_from(r: tree::RefTarget<'_>) -> Result<Expr, Self::Error> {
        let expr = match r {
            tree::RefTarget::Var(s) => Expr::Var(s.to_owned()),
            tree::RefTarget::Collection(collection) => Expr::try_from(collection)?,
            tree::RefTarget::ExprCall(call) => Expr::try_from(call)?,
            tree::RefTarget::ArrayCompr(compr) => Expr::try_from(compr)?,
            tree::RefTarget::SetCompr(compr) => Expr::try_from(compr)?,
            tree::RefTarget::ObjectCompr(compr) => Expr::try_from(compr)?,
        };
        Ok(expr)
    }
}

impl TryFrom<tree::RefArg<'_>> for Expr {
    type Error = Error;

    fn try_from(r: tree::RefArg<'_>) -> Result<Expr, Self::Error> {
        let expr = match r {
            tree::RefArg::Scalar(v) => Expr::Scalar(v),
            tree::RefArg::Var(s) => Expr::Var(s.to_owned()),
            tree::RefArg::Collection(collection) => Expr::try_from(collection)?,
            tree::RefArg::Anon => return Err(Error::Unsupported("anonymous variables")),
        };
        Ok(expr)
    }
}

impl TryFrom<tree::Collection<'_>> for Expr {
    type Error = Error;

    fn try_from(collection: tree::Collection<'_>) -> Result<Self, Self::Error> {
        let collection = match collection {
            tree::Collection::Array(items) => {
                let mut new_items = Vec::with_capacity(items.len());
                for item in items {
                    new_items.push(Expr::try_from(item)?);
                }
                Collection::Array(new_items)
            }
            tree::Collection::Set(items) => {
                let mut new_items = Vec::with_capacity(items.len());
                for item in items {
                    new_items.push(Expr::try_from(item)?);
                }
                Collection::Set(new_items)
            }
            tree::Collection::Object(items) => {
                let mut new_items = Vec::with_capacity(items.len());
                for (key, val) in items {
                    new_items.push((Expr::try_from(key)?, Expr::try_from(val)?));
                }
                Collection::Object(new_items)
            }
        };
        Ok(Expr::Collection(collection))
    }
}

impl TryFrom<tree::ExprCall<'_>> for Expr {
    type Error = Error;

    fn try_from(call: tree::ExprCall<'_>) -> Result<Self, Self::Error> {
        let (target, args) = call.into_parts();
        let target = Expr::try_from(target)?;
        let mut new_args = Vec::with_capacity(args.len());
        for arg in args {
            new_args.push(Expr::try_from(arg)?);
        }
        let mut items = Vec::new();
        items.push(target);
        items.append(&mut new_args);
        // TODO fix this
        Ok(Expr::Call("hello".to_string(), items))
    }
}

impl TryFrom<tree::ArrayCompr<'_>> for Expr {
    type Error = Error;

    fn try_from(compr: tree::ArrayCompr<'_>) -> Result<Self, Self::Error> {
        let (term, body) = compr.into_parts();
        Ok(Expr::Comprehension(Comprehension::Array(
            Box::new(Expr::try_from(term)?),
            Box::new(Expr::try_from(body)?),
        )))
    }
}

impl TryFrom<tree::SetCompr<'_>> for Expr {
    type Error = Error;

    fn try_from(compr: tree::SetCompr<'_>) -> Result<Self, Self::Error> {
        let (term, body) = compr.into_parts();
        Ok(Expr::Comprehension(Comprehension::Set(
            Box::new(Expr::try_from(term)?),
            Box::new(Expr::try_from(body)?),
        )))
    }
}

impl TryFrom<tree::ObjectCompr<'_>> for Expr {
    type Error = Error;

    fn try_from(compr: tree::ObjectCompr<'_>) -> Result<Self, Error> {
        let ((key, value), body) = compr.into_parts();
        Ok(Expr::Comprehension(Comprehension::Object(
            Box::new((Expr::try_from(key)?, Expr::try_from(value)?)),
            Box::new(Expr::try_from(body)?),
        )))
    }
}

impl TryFrom<tree::Query<'_>> for Expr {
    type Error = Error;

    fn try_from(query: tree::Query<'_>) -> Result<Self, Self::Error> {
        let statements = query.into_statements();
        if statements.len() > 0 {
            let mut statements = statements.into_iter();
            let head = statements.next().expect("len > 0");
            statements.fold(Expr::try_from(head), |acc, s| match acc {
                Ok(acc) => Ok(Expr::BinOp(
                    Box::new(acc),
                    Opcode::And,
                    Box::new(Expr::try_from(s)?),
                )),
                e => e,
            })
        } else {
            Ok(Expr::Scalar(Value::Bool(true)))
        }
    }
}

impl TryFrom<tree::Statement<'_>> for Expr {
    type Error = Error;

    fn try_from(statement: tree::Statement<'_>) -> Result<Self, Self::Error> {
        let (target, _with) = statement.into_parts();
        let e = match target {
            tree::StatementTarget::Expr(t) => Expr::try_from(t)?,
            tree::StatementTarget::NotExpr(t) => Expr::Not(Box::new(Expr::try_from(t)?)),
            tree::StatementTarget::Some(v) => {
                Expr::Some(v.into_iter().map(|s| s.to_owned()).collect())
            }
        };
        Ok(e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::parse_module;

    #[test]
    fn test_convert_module() {
        let input = r###"
        package opa.examples
        import data.servers

        violations[blah] { true }

        violations[server] {
            # ...the server exists
            server = servers[i]
            # ...and any of the server's protocols is HTTP
            server.protocols[j] = "http"
            # ...and the server is public.
            public_servers[server]
        }
        "###;
        let module = parse_module(input).unwrap();
        let module = Module::try_from(module).unwrap();
        println!("{:?}", module);
    }

    #[test]
    fn test_conflicting() {
        let cases = [
            (
                "package test; complete = 4; complete = 5;",
                Error::ConflictingRules("complete".to_string()),
            ),
            (
                "package test; complete[key] = 4 { true }; complete[key] { true };",
                Error::ConflictingRules("complete".to_string()),
            ),
        ];

        for (input, expected_err) in &cases {
            let module = parse_module(input).unwrap();
            let error = Module::try_from(module).unwrap_err();
            assert_eq!(expected_err, &error);
        }
    }
}
