use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct Module<'input> {
    package: Vec<&'input str>,
    imports: Vec<Import<'input>>,
    rules: Vec<Rule<'input>>,
}

impl<'input> Module<'input> {
    pub fn new(
        package: Vec<&'input str>,
        imports: Vec<Import<'input>>,
        rules: Vec<Rule<'input>>,
    ) -> Self {
        Self {
            package,
            imports,
            rules,
        }
    }

    pub fn into_parts(self) -> (Vec<&'input str>, Vec<Import<'input>>, Vec<Rule<'input>>) {
        (self.package, self.imports, self.rules)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import<'input> {
    term: Ref<'input>,
    as_term: Option<&'input str>,
}

impl<'input> Import<'input> {
    pub fn new(term: Ref<'input>, as_term: Option<&'input str>) -> Self {
        Self { term, as_term }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Rule<'input> {
    Default(DefaultRule<'input>),
    Complete(CompleteRule<'input>),
    Set(SetRule<'input>),
    Object(ObjectRule<'input>),
    Function(FunctionRule<'input>),
}

impl<'input> Rule<'input> {
    pub fn name(&self) -> &'input str {
        match self {
            Rule::Default(d) => d.name,
            Rule::Complete(c) => c.name,
            Rule::Set(s) => s.name,
            Rule::Object(o) => o.name,
            Rule::Function(f) => f.name,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefaultRule<'input> {
    name: &'input str,
    term: Term<'input>,
}

impl<'input> DefaultRule<'input> {
    pub fn new(name: &'input str, term: Term<'input>) -> Self {
        Self { name, term }
    }

    pub fn into_term(self) -> Term<'input> {
        self.term
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompleteRule<'input> {
    name: &'input str,
    value: Option<Term<'input>>,
    body: Option<RuleBody<'input>>,
}

impl<'input> CompleteRule<'input> {
    pub fn new(
        name: &'input str,
        value: Option<Term<'input>>,
        body: Option<RuleBody<'input>>,
    ) -> Self {
        Self { name, value, body }
    }

    pub fn into_parts(self) -> (Option<Term<'input>>, Option<RuleBody<'input>>) {
        (self.value, self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SetRule<'input> {
    name: &'input str,
    key: Term<'input>,
    body: Option<RuleBody<'input>>,
}

impl<'input> SetRule<'input> {
    pub fn new(name: &'input str, key: Term<'input>, body: Option<RuleBody<'input>>) -> Self {
        Self { name, key, body }
    }

    pub fn into_parts(self) -> (Term<'input>, Option<RuleBody<'input>>) {
        (self.key, self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectRule<'input> {
    name: &'input str,
    key: Term<'input>,
    value: Term<'input>,
    body: Option<RuleBody<'input>>,
}

impl<'input> ObjectRule<'input> {
    pub fn new(
        name: &'input str,
        key: Term<'input>,
        value: Term<'input>,
        body: Option<RuleBody<'input>>,
    ) -> Self {
        Self {
            name,
            key,
            value,
            body,
        }
    }

    pub fn into_parts(self) -> (Term<'input>, Term<'input>, Option<RuleBody<'input>>) {
        (self.key, self.value, self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionRule<'input> {
    name: &'input str,
    args: Vec<Term<'input>>,
    value: Term<'input>,
    body: Option<RuleBody<'input>>,
}

impl<'input> FunctionRule<'input> {
    pub fn new(
        name: &'input str,
        args: Vec<Term<'input>>,
        value: Term<'input>,
        body: Option<RuleBody<'input>>,
    ) -> Self {
        Self {
            name,
            args,
            value,
            body,
        }
    }

    pub fn into_parts(self) -> (Vec<Term<'input>>, Term<'input>, Option<RuleBody<'input>>) {
        (self.args, self.value, self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RuleBody<'input> {
    Clauses(Vec<Query<'input>>),
    WithElses(Query<'input>, Vec<Else<'input>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Else<'input> {
    value: Option<Term<'input>>,
    query: Query<'input>,
}

impl<'input> Else<'input> {
    pub fn new(value: Option<Term<'input>>, query: Query<'input>) -> Self {
        Self { value, query }
    }

    pub fn into_parts(self) -> (Option<Term<'input>>, Query<'input>) {
        (self.value, self.query)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Query<'input> {
    statements: Vec<Statement<'input>>,
}

impl<'input> Query<'input> {
    pub fn new(statements: Vec<Statement<'input>>) -> Self {
        Self { statements }
    }

    pub fn into_statements(self) -> Vec<Statement<'input>> {
        self.statements
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

    pub fn into_parts(self) -> (StatementTarget<'input>, Vec<With<'input>>) {
        (self.target, self.with)
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
    Scalar(Value),
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

    pub fn target(&self) -> &RefTarget<'input> {
        &self.target
    }

    pub fn args(&self) -> &[RefArg<'input>] {
        &self.args
    }

    pub fn into_parts(self) -> (Box<RefTarget<'input>>, Vec<RefArg<'input>>) {
        (self.target, self.args)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RefTarget<'input> {
    Var(&'input str),
    Collection(Collection<'input>),
    ExprCall(ExprCall<'input>),
    ArrayCompr(ArrayCompr<'input>),
    SetCompr(SetCompr<'input>),
    ObjectCompr(ObjectCompr<'input>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum RefArg<'input> {
    Collection(Collection<'input>),
    Var(&'input str),
    Scalar(Value),
    Anon,
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

    pub fn into_parts(self) -> (Ref<'input>, Vec<Term<'input>>) {
        (self.target, self.args)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Collection<'input> {
    Array(Vec<Term<'input>>),
    Set(Vec<Term<'input>>),
    Object(Vec<(Term<'input>, Term<'input>)>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayCompr<'input> {
    term: Term<'input>,
    body: Query<'input>,
}

impl<'input> ArrayCompr<'input> {
    pub fn new(term: Term<'input>, body: Query<'input>) -> Self {
        Self { term, body }
    }

    pub fn into_parts(self) -> (Term<'input>, Query<'input>) {
        (self.term, self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SetCompr<'input> {
    term: Term<'input>,
    body: Query<'input>,
}

impl<'input> SetCompr<'input> {
    pub fn new(term: Term<'input>, body: Query<'input>) -> Self {
        Self { term, body }
    }

    pub fn into_parts(self) -> (Term<'input>, Query<'input>) {
        (self.term, self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectCompr<'input> {
    item: (Term<'input>, Term<'input>),
    body: Query<'input>,
}

impl<'input> ObjectCompr<'input> {
    pub fn new(item: (Term<'input>, Term<'input>), body: Query<'input>) -> Self {
        Self { item, body }
    }

    pub fn into_parts(self) -> ((Term<'input>, Term<'input>), Query<'input>) {
        (self.item, self.body)
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
