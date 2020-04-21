use lalrpop_util::lalrpop_mod;

lalrpop_mod!(grammar);
mod lexer;
mod token;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
