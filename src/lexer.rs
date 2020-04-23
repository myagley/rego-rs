use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

use ordered_float::NotNan;

use crate::token::{StringLiteral, Token};
use crate::{Location, Span, Spanned};

pub type SpannedToken<'i> = Spanned<Token<'i>>;

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    NonParseableInt,
    NonParseableFloat,
    UnexpectedChar(char),
    UnexpectedEof,
    UnrecognizedOperator,
    UnrecognizedToken,
    UnterminatedStringLiteral,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ErrorKind::*;
        match self {
            NonParseableInt => write!(f, "cannot parse integer. most likely overflows an int64"),
            NonParseableFloat => write!(f, "cannot parse float. most likely is NaN"),
            UnexpectedChar(c) => write!(f, "unexpected character: {}", c),
            UnexpectedEof => write!(f, "unexpected end of file"),
            UnrecognizedOperator => write!(f, "unrecognized operator"),
            UnrecognizedToken => write!(f, "unrecognized token"),
            UnterminatedStringLiteral => write!(f, "unterminated string literal"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    span: Span,
    kind: ErrorKind,
}

impl Error {
    fn new(span: Span, kind: ErrorKind) -> Self {
        Self { span, kind }
    }

    fn at_location(location: Location, kind: ErrorKind) -> Self {
        let span = Span::new(location, location);
        Self::new(span, kind)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "error {}: {}", self.span, self.kind)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

struct CharLocations<'input> {
    location: Location,
    chars: Chars<'input>,
}

impl<'input> CharLocations<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            location: Location::default(),
            chars: input.chars(),
        }
    }
}

impl<'input> Iterator for CharLocations<'input> {
    type Item = (Location, char);

    fn next(&mut self) -> Option<Self::Item> {
        self.chars.next().map(|ch| {
            let location = self.location;
            self.location.shift(ch);
            (location, ch)
        })
    }
}

pub struct Lexer<'input> {
    chars: Peekable<CharLocations<'input>>,
    loc: Location,
    input: &'input str,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        let chars = CharLocations::new(input);
        Self {
            chars: chars.peekable(),
            loc: Location::default(),
            input,
        }
    }

    fn bump(&mut self) -> Option<(Location, char)> {
        self.chars.next().map(|(loc, ch)| {
            self.loc = loc;
            (loc, ch)
        })
    }

    fn peek(&mut self) -> Option<(Location, char)> {
        self.chars.peek().copied()
    }

    fn test_peek<F>(&mut self, mut test: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        self.peek().map_or(false, |(_, ch)| test(ch))
    }

    fn take_while<F>(&mut self, start: Location, mut keep_going: F) -> (Location, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        self.take_until(start, |c| !keep_going(c))
    }

    fn take_until<F>(&mut self, start: Location, mut terminate: F) -> (Location, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        while let Some((end, ch)) = self.peek() {
            if terminate(ch) {
                return (end, self.slice(start, end));
            } else {
                self.bump();
            }
        }
        let next_loc = self.next_loc();
        (next_loc, self.slice(start, next_loc))
    }

    fn skip_to_end(&mut self) {
        while let Some(_) = self.next() {}
    }

    fn error<T>(&mut self, location: Location, kind: ErrorKind) -> Result<T, Error> {
        self.skip_to_end();
        error(location, kind)
    }

    fn eof_error<T>(&mut self) -> Result<T, Error> {
        let location = self.next_loc();
        self.error(location, ErrorKind::UnexpectedEof)
    }

    fn next_loc(&mut self) -> Location {
        let mut loc = self.loc;
        loc.shift('a');
        self.peek().map_or(loc, |l| l.0)
    }

    fn slice(&self, start: Location, end: Location) -> &'input str {
        &self.input[start.absolute.into()..end.absolute.into()]
    }

    fn escape_code(&mut self) -> Result<Location, Error> {
        match self.bump() {
            Some((loc, _ch)) => Ok(loc),
            None => self.eof_error(),
        }
    }

    fn string_literal(&mut self, start: Location) -> Result<SpannedToken<'input>, Error> {
        let content_start = self.next_loc();
        loop {
            match self.bump() {
                Some((_, '\\')) => {
                    self.escape_code()?;
                }
                Some((content_end, '"')) => {
                    let end = self.next_loc();
                    let s = StringLiteral::Escaped(self.slice(content_start, content_end));
                    return spanned(start, end, Token::StringLiteral(s));
                }
                Some((_loc, _ch)) => continue,
                None => break,
            }
        }
        self.error(start, ErrorKind::UnterminatedStringLiteral)
    }

    fn raw_string_literal(&mut self, start: Location) -> Result<SpannedToken<'input>, Error> {
        let content_start = self.next_loc();
        loop {
            match self.bump() {
                Some((content_end, '`')) => {
                    let end = self.next_loc();
                    let s = StringLiteral::Raw(self.slice(content_start, content_end));
                    return spanned(start, end, Token::StringLiteral(s));
                }
                Some((_loc, _ch)) => continue,
                None => break,
            }
        }
        self.error(start, ErrorKind::UnterminatedStringLiteral)
    }

    fn numeric_literal(&mut self, start: Location) -> Result<SpannedToken<'input>, Error> {
        let (end, int) = self.take_while(start, is_digit);
        let (start, end, token) = match self.peek() {
            Some((_, '.')) => {
                self.bump();
                let (end, float) = self.take_while(start, is_digit);
                let (end, float) = match self.peek() {
                    Some((_, 'e')) | Some((_, 'E')) => self.exponent(start),
                    _ => (end, float),
                };
                let token = float
                    .parse()
                    .map_err(|_e| Error::at_location(start, ErrorKind::NonParseableFloat))
                    .and_then(|f| {
                        NotNan::new(f)
                            .map_err(|_e| Error::at_location(start, ErrorKind::NonParseableFloat))
                    })
                    .map(Token::FloatLiteral)?;
                self.error_if_next(is_ident_start)?;
                (start, end, token)
            }
            Some((_, 'e')) | Some((_, 'E')) => {
                let (end, float) = self.exponent(start);
                let token = float
                    .parse()
                    .map_err(|_e| Error::at_location(start, ErrorKind::NonParseableFloat))
                    .and_then(|f| {
                        NotNan::new(f)
                            .map_err(|_e| Error::at_location(start, ErrorKind::NonParseableFloat))
                    })
                    .map(Token::FloatLiteral)?;
                self.error_if_next(is_ident_start)?;
                (start, end, token)
            }
            Some((start, ch)) if is_ident_start(ch) => {
                return self.error(start, ErrorKind::UnexpectedChar(ch))
            }
            None | Some(_) => {
                let token = int
                    .parse()
                    .map(Token::IntLiteral)
                    .map_err(|_e| Error::at_location(start, ErrorKind::NonParseableInt))?;
                (start, end, token)
            }
        };
        spanned(start, end, token)
    }

    fn exponent(&mut self, start: Location) -> (Location, &'input str) {
        self.bump();
        match self.peek() {
            Some((_, '+')) | Some((_, '-')) => {
                self.bump();
                self.take_while(start, is_digit)
            }
            _ => self.take_while(start, is_digit),
        }
    }

    fn identifier(&mut self, start: Location) -> Result<SpannedToken<'input>, Error> {
        let (end, ident) = self.take_while(start, is_ident_continue);
        let token = match ident {
            "as" => Token::As,
            "default" => Token::Default,
            "else" => Token::Else,
            "false" => Token::False,
            "import" => Token::Import,
            "not" => Token::Not,
            "null" => Token::Null,
            "package" => Token::Package,
            "set" => Token::Set,
            "some" => Token::Some,
            "true" => Token::True,
            "with" => Token::With,
            "_" => Token::UnderScore,
            src => Token::Identifier(src),
        };
        spanned(start, end, token)
    }

    fn operator(&mut self, start: Location) -> Result<SpannedToken<'input>, Error> {
        let (end, op) = self.take_while(start, |ch| ch == '=');
        let token = match op {
            "+" => Token::Plus,
            "-" => Token::Minus,
            "*" => Token::Star,
            "/" => Token::Slash,
            "." => Token::Dot,
            ">" => Token::Greater,
            ">=" => Token::GreaterEqual,
            "<" => Token::Less,
            "<=" => Token::LessEqual,
            "=" => Token::Equal,
            "==" => Token::EqualEqual,
            "!=" => Token::NotEqual,
            ":" => Token::Colon,
            ":=" => Token::Assign,
            ";" => Token::SemiColon,
            "," => Token::Comma,
            "&" => Token::Amper,
            "|" => Token::Vbar,
            _ => {
                let span = Span::new(start, end);
                return Err(Error::new(span, ErrorKind::UnrecognizedOperator));
            }
        };
        spanned(start, end, token)
    }

    fn error_if_next<F>(&mut self, mut test: F) -> Result<(), Error>
    where
        F: FnMut(char) -> bool,
    {
        if let Some((start, ch)) = self.peek() {
            if test(ch) {
                return self.error(start, ErrorKind::UnexpectedChar(ch));
            }
        }
        Ok(())
    }

    fn next_token(&mut self) -> Option<Result<SpannedToken<'input>, Error>> {
        loop {
            return match self.bump() {
                Some((loc, '\n')) => Some(spanned(loc, self.next_loc(), Token::Newline)),
                Some((loc, '\r')) => Some(spanned(loc, self.next_loc(), Token::CarriageReturn)),
                Some((loc, '{')) => Some(spanned(loc, self.next_loc(), Token::BraceOpen)),
                Some((loc, '}')) => Some(spanned(loc, self.next_loc(), Token::BraceClose)),
                Some((loc, '[')) => Some(spanned(loc, self.next_loc(), Token::BracketOpen)),
                Some((loc, ']')) => Some(spanned(loc, self.next_loc(), Token::BracketClose)),
                Some((loc, '(')) => Some(spanned(loc, self.next_loc(), Token::ParenOpen)),
                Some((loc, ')')) => Some(spanned(loc, self.next_loc(), Token::ParenClose)),
                Some((start, '"')) => Some(self.string_literal(start)),
                Some((start, '`')) => Some(self.raw_string_literal(start)),
                Some((start, '#')) => {
                    self.take_until(start, |ch| ch == '\n');
                    continue;
                }
                Some((start, ch)) if is_ident_start(ch) => Some(self.identifier(start)),
                Some((start, ch)) if is_digit(ch) || (ch == '-' && self.test_peek(is_digit)) => {
                    Some(self.numeric_literal(start))
                }
                Some((start, ch)) if is_operator(ch) => Some(self.operator(start)),
                Some((_, ch)) if ch.is_whitespace() => continue,
                Some((loc, _ch)) => Some(self.error(loc, ErrorKind::UnrecognizedToken)),
                None => None,
            };
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<(Location, Token<'input>, Location), Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token().map(|result| {
            result.map(|spanned| {
                let (token, span) = spanned.into_parts();
                (span.start(), token, span.end())
            })
        })
    }
}

fn error<T>(location: Location, kind: ErrorKind) -> Result<T, Error> {
    let span = Span::new(location, location);
    let e = Error { span, kind };
    Err(e)
}

fn spanned<'input>(
    start: Location,
    end: Location,
    token: Token<'input>,
) -> Result<SpannedToken, Error> {
    let span = Span::new(start, end);
    Ok(Spanned::new(token, span))
}

fn is_digit(ch: char) -> bool {
    ch.is_digit(10)
}

fn is_ident_start(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_ident_continue(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

pub fn is_operator(c: char) -> bool {
    match c {
        '+' | '-' | '*' | '/' | '.' | '>' | '<' | '=' | '!' | ':' | ';' | ',' | '&' | '|' => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unrecognized_token() {
        let mut lexer = Lexer::new("^");
        let start = Location::new(0, 0, 0);
        assert_eq!(
            Some(error(start, ErrorKind::UnrecognizedToken)),
            lexer.next_token()
        );
    }

    #[test]
    fn test_lex_string() {
        let mut lexer = Lexer::new("  \"hello, there\"");
        let start = Location::new(0, 2, 2);
        let end = Location::new(0, 16, 16);
        let expected = Token::StringLiteral(StringLiteral::Escaped("hello, there"));
        assert_eq!(Some(spanned(start, end, expected)), lexer.next_token());
    }

    #[test]
    fn test_lex_unterminated_string() {
        let mut lexer = Lexer::new("  \"hello, there");
        let start = Location::new(0, 2, 2);
        assert_eq!(
            Some(error(start, ErrorKind::UnterminatedStringLiteral)),
            lexer.next_token()
        );
    }

    #[test]
    fn test_lex_unterminated_escapecode() {
        let mut lexer = Lexer::new("  \"hello, th\\");
        let start = Location::new(0, 13, 13);
        assert_eq!(
            Some(error(start, ErrorKind::UnexpectedEof)),
            lexer.next_token()
        );
    }

    #[test]
    fn test_lex_raw_string() {
        let mut lexer = Lexer::new("  `hello, there`");
        let start = Location::new(0, 2, 2);
        let end = Location::new(0, 16, 16);
        let expected = Token::StringLiteral(StringLiteral::Raw("hello, there"));
        assert_eq!(Some(spanned(start, end, expected)), lexer.next_token());
    }

    #[test]
    fn test_lex_unterminated_raw_string() {
        let mut lexer = Lexer::new("  `hello, there");
        let start = Location::new(0, 2, 2);
        assert_eq!(
            Some(error(start, ErrorKind::UnterminatedStringLiteral)),
            lexer.next_token()
        );
    }

    #[test]
    fn test_lex_integer() {
        let mut lexer = Lexer::new("  1234 ");
        let start = Location::new(0, 2, 2);
        let end = Location::new(0, 6, 6);
        let expected = Token::IntLiteral(1234);
        assert_eq!(Some(spanned(start, end, expected)), lexer.next_token());
    }

    #[test]
    fn test_lex_integer2() {
        let mut lexer = Lexer::new(" 123");
        let start = Location::new(0, 1, 1);
        let end = Location::new(0, 4, 4);
        let expected = Token::IntLiteral(123);
        assert_eq!(Some(spanned(start, end, expected)), lexer.next_token());
    }

    #[test]
    fn test_lex_float() {
        let mut lexer = Lexer::new("  -123.4 ");
        let start = Location::new(0, 2, 2);
        let end = Location::new(0, 8, 8);
        let expected = Token::FloatLiteral(NotNan::new(-123.4).unwrap());
        assert_eq!(Some(spanned(start, end, expected)), lexer.next_token());
    }

    #[test]
    fn test_lex_float_exponent() {
        let mut lexer = Lexer::new("  123.4e1 ");
        let start = Location::new(0, 2, 2);
        let end = Location::new(0, 9, 9);
        let expected = Token::FloatLiteral(NotNan::new(1234.0).unwrap());
        assert_eq!(Some(spanned(start, end, expected)), lexer.next_token());
    }

    #[test]
    fn test_lex_float_pos_exponent() {
        let mut lexer = Lexer::new("  123.4e+1 ");
        let start = Location::new(0, 2, 2);
        let end = Location::new(0, 10, 10);
        let expected = Token::FloatLiteral(NotNan::new(1234.0).unwrap());
        assert_eq!(Some(spanned(start, end, expected)), lexer.next_token());
    }

    #[test]
    fn test_lex_float_neg_exponent() {
        let mut lexer = Lexer::new("  123.4e-1 ");
        let start = Location::new(0, 2, 2);
        let end = Location::new(0, 10, 10);
        let expected = Token::FloatLiteral(NotNan::new(12.34).unwrap());
        assert_eq!(Some(spanned(start, end, expected)), lexer.next_token());
    }

    #[test]
    fn test_lex_float_no_fraction() {
        let mut lexer = Lexer::new("  1234e-1 ");
        let start = Location::new(0, 2, 2);
        let end = Location::new(0, 9, 9);
        let expected = Token::FloatLiteral(NotNan::new(123.4).unwrap());
        assert_eq!(Some(spanned(start, end, expected)), lexer.next_token());
    }

    #[test]
    fn test_lex_number_with_ident() {
        let cases = [
            ("  1234e-1a ", Location::new(0, 9, 9)),
            ("  1234e+1a ", Location::new(0, 9, 9)),
            ("  1234e11a ", Location::new(0, 9, 9)),
            ("  123.4e1a ", Location::new(0, 9, 9)),
            ("  123a ", Location::new(0, 5, 5)),
            ("  1234e-1a ", Location::new(0, 9, 9)),
        ];

        for (input, loc) in &cases {
            let mut lexer = Lexer::new(input);
            assert_eq!(
                Some(error(*loc, ErrorKind::UnexpectedChar('a'))),
                lexer.next_token()
            );
        }
    }

    #[test]
    fn test_lex_identifiers() {
        let cases = [
            ("  as ", Token::As, Location::new(0, 4, 4)),
            ("  default ", Token::Default, Location::new(0, 9, 9)),
            ("  else ", Token::Else, Location::new(0, 6, 6)),
            ("  false ", Token::False, Location::new(0, 7, 7)),
            ("  import ", Token::Import, Location::new(0, 8, 8)),
            ("  not ", Token::Not, Location::new(0, 5, 5)),
            ("  null ", Token::Null, Location::new(0, 6, 6)),
            ("  package ", Token::Package, Location::new(0, 9, 9)),
            ("  some ", Token::Some, Location::new(0, 6, 6)),
            ("  true ", Token::True, Location::new(0, 6, 6)),
            ("  with ", Token::With, Location::new(0, 6, 6)),
            (
                "  hello ",
                Token::Identifier("hello"),
                Location::new(0, 7, 7),
            ),
        ];

        let start = Location::new(0, 2, 2);
        for (input, expected, end) in &cases {
            let mut lexer = Lexer::new(input);
            assert_eq!(Some(spanned(start, *end, *expected)), lexer.next_token())
        }
    }

    #[test]
    fn test_lex_comment() {
        let cases = [
            ("  # this is a comment", None),
            (
                "  # this is a comment\n",
                Some(spanned(
                    Location::new(0, 21, 21),
                    Location::new(0, 22, 22),
                    Token::Newline,
                )),
            ),
        ];
        for (input, expected) in &cases {
            let mut lexer = Lexer::new(input);
            assert_eq!(*expected, lexer.next_token());
        }
    }

    #[test]
    fn test_lex_operators() {
        let cases = [
            ("  + ", Token::Plus, Location::new(0, 3, 3)),
            ("  - ", Token::Minus, Location::new(0, 3, 3)),
            ("  * ", Token::Star, Location::new(0, 3, 3)),
            ("  / ", Token::Slash, Location::new(0, 3, 3)),
            ("  . ", Token::Dot, Location::new(0, 3, 3)),
            ("  > ", Token::Greater, Location::new(0, 3, 3)),
            ("  >= ", Token::GreaterEqual, Location::new(0, 4, 4)),
            ("  < ", Token::Less, Location::new(0, 3, 3)),
            ("  <= ", Token::LessEqual, Location::new(0, 4, 4)),
            ("  = ", Token::Equal, Location::new(0, 3, 3)),
            ("  == ", Token::EqualEqual, Location::new(0, 4, 4)),
            ("  := ", Token::Assign, Location::new(0, 4, 4)),
            ("  != ", Token::NotEqual, Location::new(0, 4, 4)),
            ("  : ", Token::Colon, Location::new(0, 3, 3)),
            ("  ; ", Token::SemiColon, Location::new(0, 3, 3)),
            ("  , ", Token::Comma, Location::new(0, 3, 3)),
            ("  & ", Token::Amper, Location::new(0, 3, 3)),
            ("  | ", Token::Vbar, Location::new(0, 3, 3)),
        ];

        let start = Location::new(0, 2, 2);
        for (input, expected, end) in &cases {
            let mut lexer = Lexer::new(input);
            assert_eq!(Some(spanned(start, *end, *expected)), lexer.next_token())
        }
    }

    #[test]
    fn test_locations() {
        let input = "1234\n  \"hello\"";
        let lexer = Lexer::new(input);
        let expected = vec![
            Ok((
                Location::new(0, 0, 0),
                Token::IntLiteral(1234),
                Location::new(0, 4, 4),
            )),
            Ok((
                Location::new(0, 4, 4),
                Token::Newline,
                Location::new(1, 0, 5),
            )),
            Ok((
                Location::new(1, 2, 7),
                Token::StringLiteral(StringLiteral::Escaped("hello")),
                Location::new(1, 9, 14),
            )),
        ];

        let result = lexer.collect::<Vec<Result<(Location, Token<'static>, Location), Error>>>();
        assert_eq!(expected, result);
    }
}
