use std::collections::HashSet;
use std::mem;

use crate::ast::*;
use crate::value::Value;
use crate::vm::{Error, DATA_ROOT, INPUT_ROOT};

// TODO(miyagley) handle import name resolution

/// Fully qualifies rule definitiions and references
pub struct RuleResolver {
    package: String,
    local_rules: HashSet<String>,
}

impl RuleResolver {
    pub fn new(package: &[String]) -> Self {
        Self {
            package: package.join("."),
            local_rules: HashSet::new(),
        }
    }

    fn visit_rulebody(&mut self, body: &mut RuleBody) -> Result<(), Error> {
        match body {
            RuleBody::Query(query) => query.accept(self)?,
            RuleBody::WithElses(query, elses) => {
                query.accept(self)?;
                for el in elses {
                    if let Some(value) = el.value_mut() {
                        value.accept(self)?;
                    }
                    el.query_mut().accept(self)?;
                }
            }
        }
        Ok(())
    }

    fn visit_vec(&mut self, items: &mut Vec<Expr>) -> Result<(), Error> {
        for item in items {
            item.accept(self)?;
        }
        Ok(())
    }
}

impl Visitor for RuleResolver {
    type Value = ();
    type Error = Error;

    fn visit_module(&mut self, module: &mut Module) -> Result<Self::Value, Self::Error> {
        for rule in module.rules_mut() {
            self.local_rules.insert(rule.name().to_owned());
            rule.set_name(format!("{}.{}.{}", DATA_ROOT, self.package, rule.name()));
        }

        for rule in module.rules_mut() {
            if let Some(default) = rule.default_mut() {
                default.accept(self)?;
            }

            for clause in rule.clauses_mut() {
                match clause {
                    Clause::Default { value } => value.accept(self)?,
                    Clause::Complete { value, body } => {
                        value.accept(self)?;
                        self.visit_rulebody(body)?;
                    }
                    Clause::Set { key, body } => {
                        key.accept(self)?;
                        body.accept(self)?;
                    }
                    Clause::Object { key, value, body } => {
                        key.accept(self)?;
                        value.accept(self)?;
                        body.accept(self)?;
                    }
                    Clause::Function { args, value, body } => {
                        self.visit_vec(args)?;
                        value.accept(self)?;
                        self.visit_rulebody(body)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Result<Self::Value, Self::Error> {
        match expr {
            Expr::Scalar(_) => (),
            Expr::Collection(collection) => match collection {
                Collection::Array(array) => self.visit_vec(array)?,
                Collection::Set(set) => self.visit_vec(set)?,
                Collection::Object(map) => {
                    for (key, value) in map {
                        key.accept(self)?;
                        value.accept(self)?;
                    }
                }
            },
            Expr::Comprehension(ref mut compr) => match compr {
                Comprehension::Array(head, body) => {
                    head.accept(self)?;
                    body.accept(self)?;
                }
                Comprehension::Set(head, body) => {
                    head.accept(self)?;
                    body.accept(self)?;
                }
                Comprehension::Object(head, body) => {
                    head.0.accept(self)?;
                    head.1.accept(self)?;
                    body.accept(self)?;
                }
            },
            Expr::Var(s) if self.local_rules.contains(s) => {
                let new_var = format!("{}.{}.{}", DATA_ROOT, self.package, s);
                mem::replace(expr, Expr::RuleCall(new_var));
            }
            Expr::Var(_) => {
                // local variable
            }
            Expr::VarBrack(s) if self.local_rules.contains(s) => {
                let new_var = format!("{}.{}.{}", DATA_ROOT, self.package, s);
                mem::replace(expr, Expr::RuleCall(new_var));
            }
            Expr::VarBrack(_) => {
                // local variable
            }
            Expr::InputRoot => (),
            Expr::BinOp(ref mut left, _op, ref mut right) => {
                left.accept(self)?;
                right.accept(self)?;
            }
            Expr::Index(v) => {
                match v.as_slice() {
                    &[Expr::Var(ref s), ..] if s == DATA_ROOT => {
                        // take while Var
                        let (head, mut tail): (Vec<Expr>, Vec<Expr>) = v
                            .clone()
                            .into_iter()
                            .partition(|i| matches!(i, Expr::Var(_)));
                        let rule = head
                            .iter()
                            .filter_map(|e| match e {
                                Expr::Var(s) => Some(s.as_str()),
                                _ => None,
                            })
                            .collect::<Vec<&str>>()
                            .join(".");
                        let new_expr = if tail.len() > 0 {
                            let mut items = vec![Expr::RuleCall(rule)];
                            items.append(&mut tail);
                            let mut new_expr = Expr::Index(items);
                            new_expr.accept(self)?;
                            new_expr
                        } else {
                            Expr::RuleCall(rule)
                        };
                        mem::replace(expr, new_expr);
                    }
                    // &[Expr::Var(ref s), ..] => {
                    //     // TODO(miyagley) handle imports
                    // }
                    _ => self.visit_vec(v)?,
                }
            }
            Expr::RuleCall(_) => (),
            Expr::FuncCall(_, args) => self.visit_vec(args)?,
            Expr::Not(not) => not.accept(self)?,
            Expr::Some(_) => (),
        }
        Ok(())
    }
}

/// Resolves `input` and converts dotted indexes to scalar string indexes
pub struct InputResolver;

impl InputResolver {
    pub fn new() -> Self {
        Self
    }

    fn visit_vec(&mut self, items: &mut Vec<Expr>) -> Result<(), Error> {
        for item in items {
            item.accept(self)?;
        }
        Ok(())
    }
}

impl Visitor for InputResolver {
    type Value = ();
    type Error = Error;

    fn visit_module(&mut self, _module: &mut Module) -> Result<Self::Value, Self::Error> {
        Ok(())
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Result<Self::Value, Self::Error> {
        match expr {
            Expr::Scalar(_) => (),
            Expr::Collection(collection) => match collection {
                Collection::Array(array) => self.visit_vec(array)?,
                Collection::Set(set) => self.visit_vec(set)?,
                Collection::Object(map) => {
                    for (key, value) in map {
                        key.accept(self)?;
                        value.accept(self)?;
                    }
                }
            },
            Expr::Comprehension(ref mut compr) => match compr {
                Comprehension::Array(head, body) => {
                    head.accept(self)?;
                    body.accept(self)?;
                }
                Comprehension::Set(head, body) => {
                    head.accept(self)?;
                    body.accept(self)?;
                }
                Comprehension::Object(head, body) => {
                    head.0.accept(self)?;
                    head.1.accept(self)?;
                    body.accept(self)?;
                }
            },
            Expr::Var(s) if s == INPUT_ROOT => {
                mem::replace(expr, Expr::InputRoot);
            }
            Expr::Var(_) => {
                // local variable
            }
            Expr::VarBrack(s) if s == INPUT_ROOT => {
                mem::replace(expr, Expr::InputRoot);
            }
            Expr::VarBrack(_) => {
                // local variable
            }
            Expr::InputRoot => (),
            Expr::BinOp(ref mut left, _op, ref mut right) => {
                left.accept(self)?;
                right.accept(self)?;
            }
            Expr::Index(v) => match v.as_slice() {
                &[Expr::Var(ref s), ref tail @ ..] if s == INPUT_ROOT => {
                    let mut items = vec![Expr::InputRoot];
                    for item in tail {
                        if let Expr::Var(v) = item {
                            let new_var = Expr::Scalar(Value::String(v.clone()));
                            items.push(new_var);
                        } else {
                            items.push(item.clone());
                        }
                    }
                    mem::replace(expr, Expr::Index(items));
                }
                _ => self.visit_vec(v)?,
            },
            Expr::RuleCall(_) => (),
            Expr::FuncCall(_, args) => self.visit_vec(args)?,
            Expr::Not(not) => not.accept(self)?,
            Expr::Some(_) => (),
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::hash_map::RandomState;
    use std::convert::TryFrom;
    use std::iter::FromIterator;

    use crate::parser::parse_module;
    use crate::value::Value;

    use super::*;

    #[test]
    fn test_rule_expansion() {
        let input = r###"
        package opa.examples

        a := 3
        b[out] { true }
        c[key] = value { true }
        "###;

        let rules = vec![
            Rule::new(
                "data.opa.examples.b".to_string(),
                None,
                vec![Clause::Set {
                    key: Expr::Var("out".to_string()),
                    body: Expr::Scalar(Value::Bool(true)),
                }],
            ),
            Rule::new(
                "data.opa.examples.c".to_string(),
                None,
                vec![Clause::Object {
                    key: Expr::Var("key".to_string()),
                    value: Expr::Var("value".to_string()),
                    body: Expr::Scalar(Value::Bool(true)),
                }],
            ),
            Rule::new(
                "data.opa.examples.a".to_string(),
                None,
                vec![Clause::Complete {
                    value: Expr::Scalar(Value::Bool(true)),
                    body: RuleBody::Query(Expr::Scalar(Value::Bool(true))),
                }],
            ),
        ];
        let expected = Module::new(vec!["opa".to_string(), "examples".to_string()], rules);

        let mut module = Module::try_from(parse_module(input).unwrap()).unwrap();
        let mut visitor = RuleResolver::new(module.package());
        module.accept(&mut visitor).unwrap();

        let expected = expected
            .rules()
            .iter()
            .map(|r| r.name().to_owned())
            .collect::<HashSet<String>>();
        let result = module
            .rules()
            .iter()
            .map(|r| r.name().to_owned())
            .collect::<HashSet<String>>();
        assert_eq!(expected, result);

        assert_eq!(
            HashSet::from_iter(vec!["a".to_string(), "b".to_string(), "c".to_string()]),
            visitor.local_rules
        );
    }

    #[test]
    fn test_rule_expr_expansion() {
        let input = r###"
        package opa.examples

        a = 2 { b == 3 }
        b := 3
        "###;

        let rules = vec![
            Rule::new(
                "data.opa.examples.b".to_string(),
                None,
                vec![Clause::Complete {
                    value: Expr::Scalar(Value::Number(3.into())),
                    body: RuleBody::Query(Expr::Scalar(Value::Bool(true))),
                }],
            ),
            Rule::new(
                "data.opa.examples.a".to_string(),
                None,
                vec![Clause::Complete {
                    value: Expr::Scalar(Value::Number(2.into())),
                    body: RuleBody::Query(Expr::BinOp(
                        Box::new(Expr::RuleCall("data.opa.examples.b".to_string())),
                        Opcode::EqEq,
                        Box::new(Expr::Scalar(Value::Number(3.into()))),
                    )),
                }],
            ),
        ];
        let expected = Module::new(vec!["opa".to_string(), "examples".to_string()], rules);

        let mut module = Module::try_from(parse_module(input).unwrap()).unwrap();
        let mut visitor = RuleResolver::new(module.package());
        module.accept(&mut visitor).unwrap();

        let expected: HashSet<Rule, RandomState> =
            HashSet::from_iter(expected.rules().iter().cloned());
        let result = HashSet::from_iter(module.rules().iter().cloned());
        assert_eq!(expected, result);

        assert_eq!(
            HashSet::from_iter(vec!["a".to_string(), "b".to_string()]),
            visitor.local_rules
        );
    }

    #[test]
    fn test_rule_indexed_expr_expansion() {
        let input = r###"
        package opa.examples

        a = 2 { data.opa.examples.b == 3 }
        b := 3
        "###;

        let rules = vec![
            Rule::new(
                "data.opa.examples.b".to_string(),
                None,
                vec![Clause::Complete {
                    value: Expr::Scalar(Value::Number(3.into())),
                    body: RuleBody::Query(Expr::Scalar(Value::Bool(true))),
                }],
            ),
            Rule::new(
                "data.opa.examples.a".to_string(),
                None,
                vec![Clause::Complete {
                    value: Expr::Scalar(Value::Number(2.into())),
                    body: RuleBody::Query(Expr::BinOp(
                        Box::new(Expr::RuleCall("data.opa.examples.b".to_string())),
                        Opcode::EqEq,
                        Box::new(Expr::Scalar(Value::Number(3.into()))),
                    )),
                }],
            ),
        ];
        let expected = Module::new(vec!["opa".to_string(), "examples".to_string()], rules);

        let mut module = Module::try_from(parse_module(input).unwrap()).unwrap();
        let mut visitor = RuleResolver::new(module.package());
        module.accept(&mut visitor).unwrap();

        let expected: HashSet<Rule, RandomState> =
            HashSet::from_iter(expected.rules().iter().cloned());
        let result = HashSet::from_iter(module.rules().iter().cloned());
        assert_eq!(expected, result);

        assert_eq!(
            HashSet::from_iter(vec!["a".to_string(), "b".to_string()]),
            visitor.local_rules
        );
    }

    #[test]
    fn test_rule_indexed_with_index_expr_expansion() {
        let input = r###"
        package opa.examples

        a = 2 { data.opa.examples.b[c] == 3 }
        b := 3
        c := 4
        "###;

        let rules = vec![
            Rule::new(
                "data.opa.examples.b".to_string(),
                None,
                vec![Clause::Complete {
                    value: Expr::Scalar(Value::Number(3.into())),
                    body: RuleBody::Query(Expr::Scalar(Value::Bool(true))),
                }],
            ),
            Rule::new(
                "data.opa.examples.c".to_string(),
                None,
                vec![Clause::Complete {
                    value: Expr::Scalar(Value::Number(4.into())),
                    body: RuleBody::Query(Expr::Scalar(Value::Bool(true))),
                }],
            ),
            Rule::new(
                "data.opa.examples.a".to_string(),
                None,
                vec![Clause::Complete {
                    value: Expr::Scalar(Value::Number(2.into())),
                    body: RuleBody::Query(Expr::BinOp(
                        Box::new(Expr::Index(vec![
                            Expr::RuleCall("data.opa.examples.b".to_string()),
                            Expr::RuleCall("data.opa.examples.c".to_string()),
                        ])),
                        Opcode::EqEq,
                        Box::new(Expr::Scalar(Value::Number(3.into()))),
                    )),
                }],
            ),
        ];
        let expected = Module::new(vec!["opa".to_string(), "examples".to_string()], rules);

        let mut module = Module::try_from(parse_module(input).unwrap()).unwrap();
        let mut visitor = RuleResolver::new(module.package());
        module.accept(&mut visitor).unwrap();

        let expected: HashSet<Rule, RandomState> =
            HashSet::from_iter(expected.rules().iter().cloned());
        let result = HashSet::from_iter(module.rules().iter().cloned());
        assert_eq!(expected, result);

        assert_eq!(
            HashSet::from_iter(vec!["a".to_string(), "b".to_string(), "c".to_string()]),
            visitor.local_rules
        );
    }
}
