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
        self.chars.peek().map_or(false, |(_, ch)| test(*ch))
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
        let loc = self.loc;
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
            "some" => Token::Some,
            "true" => Token::True,
            "with" => Token::With,
            src => Token::Identifier(src),
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
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<SpannedToken<'input>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
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
                Some((loc, '+')) => Some(spanned(loc, self.next_loc(), Token::Plus)),
                Some((loc, '-')) => Some(spanned(loc, self.next_loc(), Token::Minus)),
                Some((loc, '*')) => Some(spanned(loc, self.next_loc(), Token::Star)),
                Some((loc, '/')) => Some(spanned(loc, self.next_loc(), Token::Slash)),
                Some((loc, '.')) => Some(spanned(loc, self.next_loc(), Token::Dot)),
                Some((_, '>')) if self.test_peek(|ch| ch == '=') => {
                    let (loc, _) = self.bump().unwrap();
                    Some(spanned(loc, self.next_loc(), Token::GreaterEqual))
                }
                Some((loc, '>')) => Some(spanned(loc, self.next_loc(), Token::Greater)),
                Some((_, '<')) if self.test_peek(|ch| ch == '=') => {
                    let (loc, _) = self.bump().unwrap();
                    Some(spanned(loc, self.next_loc(), Token::LessEqual))
                }
                Some((loc, '<')) => Some(spanned(loc, self.next_loc(), Token::Less)),
                Some((_, '=')) if self.test_peek(|ch| ch == '=') => {
                    let (loc, _) = self.bump().unwrap();
                    Some(spanned(loc, self.next_loc(), Token::EqualEqual))
                }
                Some((loc, '=')) => Some(spanned(loc, self.next_loc(), Token::Equal)),
                Some((_, '!')) if self.test_peek(|ch| ch == '=') => {
                    let (loc, _) = self.bump().unwrap();
                    Some(spanned(loc, self.next_loc(), Token::NotEqual))
                }
                Some((loc, '!')) => Some(spanned(loc, self.next_loc(), Token::Bang)),
                Some((_, ':')) if self.test_peek(|ch| ch == '=') => {
                    let (loc, _) = self.bump().unwrap();
                    Some(spanned(loc, self.next_loc(), Token::Assign))
                }
                Some((loc, ':')) => Some(spanned(loc, self.next_loc(), Token::Colon)),
                Some((loc, ';')) => Some(spanned(loc, self.next_loc(), Token::SemiColon)),
                Some((loc, ',')) => Some(spanned(loc, self.next_loc(), Token::Comma)),
                Some((loc, '&')) => Some(spanned(loc, self.next_loc(), Token::Amper)),
                Some((loc, '|')) => Some(spanned(loc, self.next_loc(), Token::Vbar)),
                Some((_, ch)) if ch.is_whitespace() => continue,
                _ => None,
            };
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_string() {
        let mut lexer = Lexer::new("  \"hello, there\"");
        let start = Location::new(0, 2, 2);
        let end = Location::new(0, 15, 15);
        let expected = Token::StringLiteral(StringLiteral::Escaped("hello, there"));
        assert_eq!(Some(spanned(start, end, expected)), lexer.next());
    }

    #[test]
    fn test_lex_unterminated_string() {
        let mut lexer = Lexer::new("  \"hello, there");
        let start = Location::new(0, 2, 2);
        assert_eq!(
            Some(error(start, ErrorKind::UnterminatedStringLiteral)),
            lexer.next()
        );
    }

    #[test]
    fn test_lex_unterminated_escapecode() {
        let mut lexer = Lexer::new("  \"hello, th\\");
        let start = Location::new(0, 12, 12);
        assert_eq!(Some(error(start, ErrorKind::UnexpectedEof)), lexer.next());
    }

    #[test]
    fn test_lex_raw_string() {
        let mut lexer = Lexer::new("  `hello, there`");
        let start = Location::new(0, 2, 2);
        let end = Location::new(0, 15, 15);
        let expected = Token::StringLiteral(StringLiteral::Raw("hello, there"));
        assert_eq!(Some(spanned(start, end, expected)), lexer.next());
    }

    #[test]
    fn test_lex_unterminated_raw_string() {
        let mut lexer = Lexer::new("  `hello, there");
        let start = Location::new(0, 2, 2);
        assert_eq!(
            Some(error(start, ErrorKind::UnterminatedStringLiteral)),
            lexer.next()
        );
    }

    #[test]
    fn test_lex_integer() {
        let mut lexer = Lexer::new("  1234 ");
        let start = Location::new(0, 2, 2);
        let end = Location::new(0, 6, 6);
        let expected = Token::IntLiteral(1234);
        assert_eq!(Some(spanned(start, end, expected)), lexer.next());
    }

    #[test]
    fn test_lex_float() {
        let mut lexer = Lexer::new("  123.4 ");
        let start = Location::new(0, 2, 2);
        let end = Location::new(0, 7, 7);
        let expected = Token::FloatLiteral(NotNan::new(123.4).unwrap());
        assert_eq!(Some(spanned(start, end, expected)), lexer.next());
    }

    #[test]
    fn test_lex_float_exponent() {
        let mut lexer = Lexer::new("  123.4e1 ");
        let start = Location::new(0, 2, 2);
        let end = Location::new(0, 9, 9);
        let expected = Token::FloatLiteral(NotNan::new(1234.0).unwrap());
        assert_eq!(Some(spanned(start, end, expected)), lexer.next());
    }

    #[test]
    fn test_lex_float_pos_exponent() {
        let mut lexer = Lexer::new("  123.4e+1 ");
        let start = Location::new(0, 2, 2);
        let end = Location::new(0, 10, 10);
        let expected = Token::FloatLiteral(NotNan::new(1234.0).unwrap());
        assert_eq!(Some(spanned(start, end, expected)), lexer.next());
    }

    #[test]
    fn test_lex_float_neg_exponent() {
        let mut lexer = Lexer::new("  123.4e-1 ");
        let start = Location::new(0, 2, 2);
        let end = Location::new(0, 10, 10);
        let expected = Token::FloatLiteral(NotNan::new(12.34).unwrap());
        assert_eq!(Some(spanned(start, end, expected)), lexer.next());
    }

    #[test]
    fn test_lex_float_no_fraction() {
        let mut lexer = Lexer::new("  1234e-1 ");
        let start = Location::new(0, 2, 2);
        let end = Location::new(0, 9, 9);
        let expected = Token::FloatLiteral(NotNan::new(123.4).unwrap());
        assert_eq!(Some(spanned(start, end, expected)), lexer.next());
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
                lexer.next()
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
            assert_eq!(Some(spanned(start, *end, *expected)), lexer.next())
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
                    Location::new(0, 21, 21),
                    Token::Newline,
                )),
            ),
        ];
        for (input, expected) in &cases {
            let mut lexer = Lexer::new(input);
            assert_eq!(*expected, lexer.next());
        }
    }
}
