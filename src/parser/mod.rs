use lalrpop_util::lalrpop_mod;

lalrpop_mod!(grammar, "/parser/grammar.rs");
pub mod lexer;
mod token;
pub mod tree;

use self::lexer::Error as LexerError;
use crate::parser::tree::{Module, Query};
use crate::Location;

pub use lexer::Lexer;
pub use token::Token;

pub type ParseError<'input> = lalrpop_util::ParseError<Location, Token<'input>, LexerError>;

pub fn parse_module<'input>(input: &'input str) -> Result<Module<'input>, ParseError<'input>> {
    let lexer = Lexer::new(input);
    grammar::ModuleParser::new().parse(input, lexer)
}

pub fn parse_query<'input>(input: &'input str) -> Result<Query<'input>, ParseError<'input>> {
    let lexer = Lexer::new(input);
    grammar::QueryParser::new().parse(input, lexer)
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::lexer::Lexer;
    use crate::parser::tree::*;

    #[test]
    fn test_expr_parse() {
        let cases = [
            (" 123", Term::Scalar(123.into())),
            (" 123.4", Term::Scalar(123.4.into())),
            ("\"hello\"", Term::Scalar("hello".into())),
            (
                "x := (1 + 2) * 3 >= `sfs`",
                Term::BinOp(
                    Box::new(Term::Ref(Ref::new(Box::new(RefTarget::Var("x")), vec![]))),
                    Opcode::Assign,
                    Box::new(Term::BinOp(
                        Box::new(Term::BinOp(
                            Box::new(Term::BinOp(
                                Box::new(Term::Scalar(1.into())),
                                Opcode::Add,
                                Box::new(Term::Scalar(2.into())),
                            )),
                            Opcode::Mul,
                            Box::new(Term::Scalar(3.into())),
                        )),
                        Opcode::Gte,
                        Box::new(Term::Scalar("sfs".into())),
                    )),
                ),
            ),
        ];
        for (input, expected) in &cases {
            let lexer = Lexer::new(input);
            let result = grammar::ExprParser::new().parse(input, lexer).unwrap();
            assert_eq!(*expected, result);
        }
    }

    #[test]
    fn test_query_parse() {
        let cases = [
            " 123",
            " 123.4",
            "\"hello\"",
            "`raw-string`",
            "(1 + 2) * 3 >= `sfs`",
            "x = (1 + 2) * 3 >= `sfs`",
            "x = 3; y=4",
            "x = 3\ny=4",
            "x = 3\r\ny=4",
            "x = [2, 3, 4]",
            "x = {2, 3, \"4\"}",
            "x = set( )",
            "x = {x: 3, \"y\": 4}",
            "a.b",
            "a[b]",
            "a[\"string\"]",
            "time.now_ns()",
            "not 1 + 3",
            "some i, j",
            "some i",
            "a + b with {a:2} as b",
            "name := [name | sites[i].region == region; name := sites[i].name;]",
            "b := {x | x:=a[_]}",
            r#"
                app_to_hostnames := {app.name : hostnames |
                    app:=apps[_]
                    hostnames := [hostname |
                                    name := app.servers[_]
                                    s := sites[_].servers[_]
                                    s.name == name
                                    hostname := s.hostname]
                }
            "#,
        ];
        for input in &cases {
            if let Err(e) = parse_query(input) {
                panic!("input: {} {:?}", input, e);
            }
        }
    }

    #[test]
    fn test_module_parse_smoke() {
        let input = r####"
# This policy module belongs the opa.example package.
package opa.examples
# Refer to data.servers as servers.
import data.servers
# Refer to the data.networks as networks.
import data.networks
# Refer to the data.ports as ports.
import data.ports
# A server exists in the violations set if...
violations[server] {
    # ...the server exists
    server = servers[i]
    # ...and any of the server's protocols is HTTP
    server.protocols[j] = "http"
    # ...and the server is public.
    public_servers[server]
}
# A server exists in the public_servers set if...
public_servers[server] {
	# Semicolons are optional. Can group expressions onto one line.
    server = servers[i]; server.ports[j] = ports[k].id 	# ...and the server is connected to a port
    ports[k].networks[l] = networks[m].id 				# ...and the port is connected to a network
    networks[m].public = true							# ...and the network is public.
}
        "####;

        if let Err(e) = parse_module(input) {
            panic!("{:?}", e);
        }
    }
}
