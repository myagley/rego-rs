#[derive(Copy, Clone, Debug)]
pub enum Token<'input> {
    Null,
    Str(&'input str),
}
