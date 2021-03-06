use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;

use crate::parser::tree;
use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    ConflictingRules(String),
    MismatchedRules,
    MultipleDefaults(String),
    Unsupported(&'static str),
    UnexpectedElse,
    Assign(Expr),
    UnificationUnsupported,
    IllegalOpcodeConversion(tree::Opcode),
    IllegalFuncCall(Expr),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::ConflictingRules(s) => write!(f, "conflicting rules found for {}", s),
            Error::MismatchedRules => write!(f, "conflicting rules found"),
            Error::MultipleDefaults(s) => write!(f, "multiple default rules named {} found", s),
            Error::Unsupported(s) => write!(f, "{} are unsupported", s),
            Error::UnexpectedElse => write!(f, "unexpected 'else' in rule body"),
            Error::Assign(expr) => write!(f, "error assigning to variable: expected a var, found {:?}", expr),
            Error::UnificationUnsupported => write!(f, "unification with the '=' operator is unsupported. use ':=' for assignment or '==' for comparison"),
            Error::IllegalOpcodeConversion(opcode) => write!(f, "no opcode conversion for {:?}", opcode),
            Error::IllegalFuncCall(expr) => write!(f, "illegal function call reference: {:?}", expr),
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

    fn visit_module(&mut self, module: &mut Module) -> Result<Self::Value, Self::Error>;

    fn visit_expr(&mut self, expr: &mut Expr) -> Result<Self::Value, Self::Error>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub package: Vec<String>,
    pub rules: Vec<Rule>,
}

impl Module {
    pub fn accept<V>(&mut self, visitor: &mut V) -> Result<V::Value, V::Error>
    where
        V: Visitor,
    {
        visitor.visit_module(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rule {
    pub name: String,
    pub default: Option<Expr>,
    pub body: Body,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Body {
    Default(Expr),
    Complete(Vec<CompleteClause>),
    Set(Vec<SetClause>),
    Object(Vec<ObjectClause>),
    Function(Vec<FunctionClause>),
}

impl Body {
    pub fn len(&self) -> usize {
        match self {
            Self::Default(_) => 1,
            Self::Complete(c) => c.len(),
            Self::Set(c) => c.len(),
            Self::Object(c) => c.len(),
            Self::Function(c) => c.len(),
        }
    }
}

impl Body {
    fn append(self, clause: Clause) -> Result<Self, Error> {
        match (self, clause) {
            (Body::Default(expr), Clause::Default { .. }) => Ok(Body::Default(expr)),
            (Body::Complete(mut clauses), Clause::Complete { value, body }) => {
                let clause = CompleteClause { value, body };
                clauses.push(clause);
                Ok(Body::Complete(clauses))
            }
            (Body::Set(mut clauses), Clause::Set { key, body }) => {
                let clause = SetClause { key, body };
                clauses.push(clause);
                Ok(Body::Set(clauses))
            }
            (Body::Object(mut clauses), Clause::Object { key, value, body }) => {
                let clause = ObjectClause { key, value, body };
                clauses.push(clause);
                Ok(Body::Object(clauses))
            }
            (Body::Function(mut clauses), Clause::Function { args, value, body }) => {
                let clause = FunctionClause { args, value, body };
                clauses.push(clause);
                Ok(Body::Function(clauses))
            }
            _ => Err(Error::MismatchedRules),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CompleteClause {
    pub value: Expr,
    pub body: ClauseBody,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SetClause {
    pub key: Expr,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjectClause {
    pub key: Expr,
    pub value: Expr,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionClause {
    pub args: Vec<Expr>,
    pub value: Expr,
    pub body: ClauseBody,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Clause {
    Default {
        value: Expr,
    },
    Complete {
        value: Expr,
        body: ClauseBody,
    },
    Set {
        key: Expr,
        body: Expr,
    },
    Object {
        key: Expr,
        value: Expr,
        body: Expr,
    },
    Function {
        args: Vec<Expr>,
        value: Expr,
        body: ClauseBody,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ClauseBody {
    Query(Expr),
    WithElses(Expr, Vec<Else>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Else {
    pub value: Option<Expr>,
    pub query: Expr,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Scalar(Value<'static>),
    Collection(Collection),
    Comprehension(Comprehension),

    BinOp(Box<Expr>, Opcode, Box<Expr>),
    Assign(String, Box<Expr>),
    Index(Vec<Expr>),

    Var(String),
    VarBrack(String),
    InputRoot,

    RuleCall(String),
    FuncCall(String, Vec<Expr>),

    Not(Box<Expr>),
    Some(Vec<String>),
}

impl Expr {
    pub fn accept<V>(&mut self, visitor: &mut V) -> Result<V::Value, V::Error>
    where
        V: Visitor,
    {
        visitor.visit_expr(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Collection {
    Array(Vec<Expr>),
    Set(Vec<Expr>),
    Object(Vec<(Expr, Expr)>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Comprehension {
    Array(Box<Expr>, Box<Expr>),
    Set(Box<Expr>, Box<Expr>),
    Object(Box<(Expr, Expr)>, Box<Expr>),
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

fn is_conflicting(left: &Clause, right: &Clause) -> bool {
    match (left, right) {
        (Clause::Complete { value: a, .. }, Clause::Complete { value: b, .. }) if a == b => false,
        (Clause::Set { .. }, Clause::Set { .. }) => false,
        (Clause::Object { .. }, Clause::Object { .. }) => false,
        (Clause::Function { .. }, Clause::Function { .. }) => false,
        _ => true,
    }
}

impl TryFrom<Vec<Clause>> for Body {
    type Error = Error;

    fn try_from(clauses: Vec<Clause>) -> Result<Self, Self::Error> {
        // Check for conflicting rule heads
        let mut tocheck = clauses.into_iter();
        if let Some(first) = tocheck.next() {
            tocheck.fold(Body::try_from(first), |acc, c| match (acc, c) {
                (Err(e), _) => Err(e),
                (Ok(a), b) => a.append(b),
            })
        } else {
            Ok(Body::Complete(vec![]))
        }
    }
}

impl TryFrom<Clause> for Body {
    type Error = Error;

    fn try_from(clause: Clause) -> Result<Self, Self::Error> {
        let body = match clause {
            Clause::Default { value } => Body::Default(value),
            Clause::Complete { value, body } => {
                Body::Complete(vec![CompleteClause { value, body }])
            }
            Clause::Set { key, body } => Body::Set(vec![SetClause { key, body }]),
            Clause::Object { key, value, body } => {
                Body::Object(vec![ObjectClause { key, value, body }])
            }
            Clause::Function { args, value, body } => {
                Body::Function(vec![FunctionClause { args, value, body }])
            }
        };
        Ok(body)
    }
}

impl TryFrom<tree::DefaultRule<'_>> for Vec<Clause> {
    type Error = Error;

    fn try_from(rule: tree::DefaultRule<'_>) -> Result<Self, Self::Error> {
        let term = rule.into_term();
        let value = Expr::try_from(term)?;
        Ok(vec![Clause::Default { value }])
    }
}

impl TryFrom<tree::CompleteRule<'_>> for Vec<Clause> {
    type Error = Error;

    fn try_from(rule: tree::CompleteRule<'_>) -> Result<Self, Self::Error> {
        let (maybe_term, maybe_body) = rule.into_parts();
        let value = maybe_term
            .map(Expr::try_from)
            .unwrap_or_else(|| Ok(Expr::Scalar(Value::Bool(true))))?;

        let bodies = maybe_body
            .map(Vec::<ClauseBody>::try_from)
            .transpose()?
            .unwrap_or_else(|| vec![ClauseBody::Query(Expr::Scalar(Value::Bool(true)))]);
        let clauses = bodies
            .into_iter()
            .map(|body| Clause::Complete {
                value: value.clone(),
                body,
            })
            .collect();
        Ok(clauses)
    }
}

impl TryFrom<tree::SetRule<'_>> for Vec<Clause> {
    type Error = Error;

    fn try_from(rule: tree::SetRule<'_>) -> Result<Self, Self::Error> {
        let (key, maybe_body) = rule.into_parts();
        let key = Expr::try_from(key)?;
        let bodies = maybe_body
            .map(Vec::<Expr>::try_from)
            .transpose()?
            .unwrap_or_else(|| vec![Expr::Scalar(Value::Bool(true))]);
        let clauses = bodies
            .into_iter()
            .map(|body| Clause::Set {
                key: key.clone(),
                body,
            })
            .collect();
        Ok(clauses)
    }
}

impl TryFrom<tree::ObjectRule<'_>> for Vec<Clause> {
    type Error = Error;

    fn try_from(rule: tree::ObjectRule<'_>) -> Result<Self, Self::Error> {
        let (key, value, maybe_body) = rule.into_parts();
        let key = Expr::try_from(key)?;
        let value = Expr::try_from(value)?;
        let bodies = maybe_body
            .map(Vec::<Expr>::try_from)
            .transpose()?
            .unwrap_or_else(|| vec![Expr::Scalar(Value::Bool(true))]);
        let clauses = bodies
            .into_iter()
            .map(|body| Clause::Object {
                key: key.clone(),
                value: value.clone(),
                body,
            })
            .collect();
        Ok(clauses)
    }
}

impl TryFrom<tree::FunctionRule<'_>> for Vec<Clause> {
    type Error = Error;

    fn try_from(rule: tree::FunctionRule<'_>) -> Result<Self, Self::Error> {
        let (args, value, maybe_body) = rule.into_parts();
        let args = args
            .into_iter()
            .map(Expr::try_from)
            .collect::<Result<Vec<Expr>, Error>>()?;
        let value = Expr::try_from(value)?;
        let bodies = maybe_body
            .map(Vec::<ClauseBody>::try_from)
            .transpose()?
            .unwrap_or_else(|| vec![ClauseBody::Query(Expr::Scalar(Value::Bool(true)))]);
        let clauses = bodies
            .into_iter()
            .map(|body| Clause::Function {
                args: args.clone(),
                value: value.clone(),
                body,
            })
            .collect();
        Ok(clauses)
    }
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
            .into_iter()
            .map(|(name, rules)| {
                let default = extract_default(&rules)?
                    .map(|default| Expr::try_from(default.into_term()))
                    .transpose()?;

                let clauses = rules
                    .into_iter()
                    .filter(|r| !matches!(r, tree::Rule::Default(_)))
                    .map(|rule| {
                        let clause = match rule {
                            tree::Rule::Default(d) => Vec::<Clause>::try_from(d)?,
                            tree::Rule::Complete(c) => Vec::<Clause>::try_from(c)?,
                            tree::Rule::Set(s) => Vec::<Clause>::try_from(s)?,
                            tree::Rule::Object(o) => Vec::<Clause>::try_from(o)?,
                            tree::Rule::Function(f) => Vec::<Clause>::try_from(f)?,
                        };
                        Ok(clause)
                    })
                    .collect::<Result<Vec<Vec<Clause>>, Error>>()?
                    .into_iter()
                    .flatten()
                    .collect::<Vec<Clause>>();

                // Check for conflicting rule heads
                let mut tocheck = clauses.iter();
                if let Some(first) = tocheck.next() {
                    tocheck.fold(Ok(first), |acc, c| match (acc, c) {
                        (Err(e), _) => Err(e),
                        (Ok(a), b) => {
                            if is_conflicting(&a, &b) {
                                Err(Error::ConflictingRules(name.to_string()))
                            } else {
                                Ok(a)
                            }
                        }
                    })?;
                }

                let body = Body::try_from(clauses)?;

                let rule = Rule {
                    name: name.to_string(),
                    default,
                    body,
                };
                Ok(rule)
            })
            .collect::<Result<Vec<Rule>, Self::Error>>()?;

        let module = Module { package, rules };
        Ok(module)
    }
}

impl TryFrom<tree::RuleBody<'_>> for Vec<ClauseBody> {
    type Error = Error;

    fn try_from(body: tree::RuleBody<'_>) -> Result<Self, Self::Error> {
        match body {
            tree::RuleBody::Clauses(queries) => queries
                .into_iter()
                .map(|q| Ok(ClauseBody::Query(Expr::try_from(q)?)))
                .collect::<Result<Self, Error>>(),
            tree::RuleBody::WithElses(query, elses) => {
                let query = Expr::try_from(query)?;
                let elses = elses
                    .into_iter()
                    .map(|e| {
                        let (maybe_value, query) = e.into_parts();
                        let value = maybe_value.map(Expr::try_from).transpose()?;
                        let query = Expr::try_from(query)?;
                        Ok(Else { value, query })
                    })
                    .collect::<Result<Vec<Else>, Error>>()?;
                Ok(vec![ClauseBody::WithElses(query, elses)])
            }
        }
    }
}

impl TryFrom<tree::RuleBody<'_>> for Vec<Expr> {
    type Error = Error;

    fn try_from(body: tree::RuleBody<'_>) -> Result<Self, Self::Error> {
        match body {
            tree::RuleBody::Clauses(queries) => queries
                .into_iter()
                .map(|q| Ok(Expr::try_from(q)?))
                .collect::<Result<Self, Error>>(),
            tree::RuleBody::WithElses(_query, _elses) => Err(Error::UnexpectedElse),
        }
    }
}

impl TryFrom<tree::Opcode> for Opcode {
    type Error = Error;

    fn try_from(op: tree::Opcode) -> Result<Self, Self::Error> {
        let result = match op {
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
            opcode => return Err(Error::IllegalOpcodeConversion(opcode)),
        };
        Ok(result)
    }
}

impl TryFrom<tree::Term<'_>> for Expr {
    type Error = Error;

    fn try_from(term: tree::Term<'_>) -> Result<Self, Self::Error> {
        let expr = match term {
            tree::Term::BinOp(left, tree::Opcode::Assign, right) => {
                let left = Expr::try_from(*left)?;
                let right = Expr::try_from(*right)?;
                if let Expr::Var(var) = left {
                    Expr::Assign(var, Box::new(right))
                } else {
                    return Err(Error::Assign(left));
                }
            }
            tree::Term::BinOp(_left, tree::Opcode::Eq, _right) => {
                return Err(Error::UnificationUnsupported)
            }
            tree::Term::BinOp(left, op, right) => Expr::BinOp(
                Box::new(Expr::try_from(*left)?),
                Opcode::try_from(op)?,
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
            tree::RefArg::VarDot(s) => Expr::Var(s.to_owned()),
            tree::RefArg::VarBrack(s) => Expr::VarBrack(s.to_owned()),
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

        let result = match target {
            Expr::Var(name) => Expr::FuncCall(name.to_string(), new_args),
            Expr::Index(items) => {
                if !items.iter().all(|e| matches!(e, Expr::Var(_))) {
                    return Err(Error::IllegalFuncCall(Expr::Index(items)));
                }
                let name = items
                    .into_iter()
                    .filter_map(|expr| match expr {
                        Expr::Var(v) => Some(v),
                        _ => None,
                    })
                    .collect::<Vec<String>>()
                    .join(".");
                Expr::FuncCall(name, new_args)
            }
            expr => return Err(Error::IllegalFuncCall(expr)),
        };
        Ok(result)
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

    use crate::parser::{parse_module, parse_query};

    #[test]
    fn test_convert_module() {
        let input = r###"
        package opa.examples
        import data.servers

        violations[blah] { true } { false }

        test_else { true } else = 3 { true }

        violations[server] {
            # ...the server exists
            server := servers[i]
            # ...and any of the server's protocols is HTTP
            server.protocols[j] == "http"
            # ...and the server is public.
            public_servers[server]
        }
        "###;
        let module = parse_module(input).unwrap();
        let module = Module::try_from(module).unwrap();
        println!("{:#?}", module);
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

    #[test]
    fn test_functions() {
        let cases = [
            (
                "afunction(3, 4)",
                Ok(Expr::FuncCall(
                    "afunction".to_string(),
                    vec![
                        Expr::Scalar(Value::Number(3.into())),
                        Expr::Scalar(Value::Number(4.into())),
                    ],
                )),
            ),
            (
                "dotted.function()",
                Ok(Expr::FuncCall("dotted.function".to_string(), vec![])),
            ),
            (
                "[3]()",
                Err(Error::IllegalFuncCall(Expr::Collection(Collection::Array(
                    vec![Expr::Scalar(Value::Number(3.into()))],
                )))),
            ),
        ];

        for (input, expected) in &cases {
            let query = parse_query(input).unwrap();
            let result = Expr::try_from(query);
            assert_eq!(*expected, result);
        }
    }
}
