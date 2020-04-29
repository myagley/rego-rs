use rego::parser::parse_query;

fn main() {
    let query = parse_query("(2 + 3) - 5").unwrap();
    println!("{:?}", query);
}
