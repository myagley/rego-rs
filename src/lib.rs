use std::convert::TryFrom;

mod ast;
mod error;
mod parser;
mod value;
mod vm;

pub use error::Error;
pub use value::{from_value, to_value, Index, Map, Number, Set, ToValue, Value, ValueRef};
pub use vm::CompiledQuery;

pub fn compile<'input>(
    query: &'input str,
    modules: &[&'input str],
) -> Result<CompiledQuery, Error<'input>> {
    let query = parser::parse_query(query)?;
    let query = ast::Expr::try_from(query)?;

    let mut compiled_modules = Vec::with_capacity(modules.len());
    for module in modules {
        let module = parser::parse_module(module)?;
        let module = ast::Module::try_from(module)?;
        compiled_modules.push(module);
    }

    let compiled = CompiledQuery::compile(query, compiled_modules)?;
    Ok(compiled)
}
