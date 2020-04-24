use std::fmt;
use std::ops::Range;

use lalrpop_util::lalrpop_mod;

mod error;
lalrpop_mod!(grammar);
mod lexer;
mod token;
mod value;

pub use error::Error;
pub use lexer::Lexer;
pub use token::Token;
pub use value::Value;

/// The zero-indexed line offset into a file
#[derive(Clone, Copy, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LineIndex(usize);

impl LineIndex {
    pub const fn number(&self) -> LineNumber {
        LineNumber(self.0 + 1)
    }
}

impl fmt::Debug for LineIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LineIndex({})", self.0)
    }
}

impl fmt::Display for LineIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<LineIndex> for usize {
    fn from(index: LineIndex) -> Self {
        index.0
    }
}

impl From<usize> for LineIndex {
    fn from(num: usize) -> Self {
        LineIndex(num)
    }
}

/// The on-indexed line offset in the source file
#[derive(Clone, Copy, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LineNumber(usize);

impl From<LineNumber> for usize {
    fn from(num: LineNumber) -> Self {
        num.0
    }
}

impl From<usize> for LineNumber {
    fn from(num: usize) -> Self {
        LineNumber(num)
    }
}

impl fmt::Debug for LineNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LineNumber({})", self.0)
    }
}

impl fmt::Display for LineNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// The zero-indexed column offset into a file
#[derive(Clone, Copy, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ColumnIndex(usize);

impl ColumnIndex {
    pub const fn number(&self) -> ColumnNumber {
        ColumnNumber(self.0 + 1)
    }
}

impl fmt::Debug for ColumnIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ColumnIndex({})", self.0)
    }
}

impl fmt::Display for ColumnIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<ColumnIndex> for usize {
    fn from(index: ColumnIndex) -> Self {
        index.0
    }
}

impl From<usize> for ColumnIndex {
    fn from(num: usize) -> Self {
        ColumnIndex(num)
    }
}

/// The on-indexed column offset in the source file
#[derive(Clone, Copy, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ColumnNumber(usize);

impl From<ColumnNumber> for usize {
    fn from(num: ColumnNumber) -> Self {
        num.0
    }
}

impl From<usize> for ColumnNumber {
    fn from(num: usize) -> Self {
        ColumnNumber(num)
    }
}

impl fmt::Debug for ColumnNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ColumnNumber({})", self.0)
    }
}

impl fmt::Display for ColumnNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// The char index in a source file.
#[derive(Clone, Copy, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CharIndex(usize);

impl From<CharIndex> for usize {
    fn from(num: CharIndex) -> Self {
        num.0
    }
}

impl From<usize> for CharIndex {
    fn from(num: usize) -> Self {
        CharIndex(num)
    }
}

impl fmt::Debug for CharIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "CharIndex({})", self.0)
    }
}

impl fmt::Display for CharIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// A location in a source file.
#[derive(Clone, Copy, Default, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Location {
    line: LineIndex,
    column: ColumnIndex,
    absolute: CharIndex,
}

impl Location {
    pub fn new<L, C, A>(line: L, column: C, absolute: A) -> Self
    where
        L: Into<LineIndex>,
        C: Into<ColumnIndex>,
        A: Into<CharIndex>,
    {
        Self {
            line: line.into(),
            column: column.into(),
            absolute: absolute.into(),
        }
    }

    pub fn shift(&mut self, ch: char) {
        if ch == '\n' {
            self.line.0 += 1;
            self.column = ColumnIndex(0)
        } else {
            self.column.0 += 1;
        }
        self.absolute.0 += 1;
    }

    pub fn line(&self) -> &LineIndex {
        &self.line
    }

    pub fn column(&self) -> &ColumnIndex {
        &self.column
    }

    pub fn absolute(&self) -> &CharIndex {
        &self.absolute
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Line: {}, Column: {}",
            self.line.number(),
            self.column.number()
        )
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Span {
    start: Location,
    end: Location,
}

impl Span {
    pub fn new(start: Location, end: Location) -> Self {
        Self { start, end }
    }

    pub fn start(&self) -> Location {
        self.start
    }

    pub fn end(&self) -> Location {
        self.end
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}, {})", self.start.absolute, self.end.absolute)
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Range<usize> {
        span.start.absolute.into()..span.end.absolute.into()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Spanned<T> {
    value: T,
    span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }

    pub fn value(&self) -> &T {
        &self.value
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn into_parts(self) -> (T, Span) {
        let Self { value, span } = self;
        (value, span)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
            let lexer = Lexer::new(input);
            if let Err(e) = grammar::QueryParser::new().parse(input, lexer) {
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

        let lexer = Lexer::new(input);
        if let Err(e) = grammar::ModuleParser::new().parse(input, lexer) {
            panic!("{:?}", e);
        }
    }
}
