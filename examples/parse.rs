use rego::ast::Query;
use rego::parser::{parse_query, ParseError};

fn main() {
    let query = parse_query("(2 + 3) - 5").unwrap();
    println!("{:?}", query);
}
