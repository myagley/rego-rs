use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

use crate::token::{StringLiteral, Token};
use crate::{Location, Span, Spanned};

pub type SpannedToken<'i> = Spanned<Token<'i>>;

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    UnexpectedEof,
    UnterminatedStringLiteral,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ErrorKind::*;
        match self {
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

    fn skip_to_end(&mut self) {
        while let Some(_) = self.next() {}
    }

    fn bump(&mut self) -> Option<(Location, char)> {
        self.chars.next().map(|(loc, ch)| {
            self.loc = loc;
            (loc, ch)
        })
    }

    fn peek(&mut self) -> Option<&(Location, char)> {
        self.chars.peek()
    }

    fn test_peek<F>(&mut self, mut test: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        self.chars.peek().map_or(false, |(_, ch)| test(*ch))
    }

    fn error<T>(&mut self, kind: ErrorKind, location: Location) -> Result<T, Error> {
        self.skip_to_end();
        error(kind, location)
    }

    fn eof_error<T>(&mut self) -> Result<T, Error> {
        let location = self.next_loc();
        self.error(ErrorKind::UnexpectedEof, location)
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
        self.error(ErrorKind::UnterminatedStringLiteral, start)
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<SpannedToken<'input>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.bump() {
                Some((loc, '\n')) => return Some(spanned(loc, self.next_loc(), Token::Newline)),
                Some((loc, '\r')) => {
                    return Some(spanned(loc, self.next_loc(), Token::CarriageReturn))
                }
                Some((_, ch)) if ch.is_whitespace() => continue,
                Some((loc, '{')) => return Some(spanned(loc, self.next_loc(), Token::BraceOpen)),
                Some((loc, '}')) => return Some(spanned(loc, self.next_loc(), Token::BraceClose)),
                Some((loc, '[')) => return Some(spanned(loc, self.next_loc(), Token::BracketOpen)),
                Some((loc, ']')) => {
                    return Some(spanned(loc, self.next_loc(), Token::BracketClose))
                }
                Some((loc, '(')) => return Some(spanned(loc, self.next_loc(), Token::ParenOpen)),
                Some((loc, ')')) => return Some(spanned(loc, self.next_loc(), Token::ParenClose)),
                Some((loc, '+')) => return Some(spanned(loc, self.next_loc(), Token::Plus)),
                Some((loc, '-')) => return Some(spanned(loc, self.next_loc(), Token::Minus)),
                Some((loc, '*')) => return Some(spanned(loc, self.next_loc(), Token::Star)),
                Some((loc, '/')) => return Some(spanned(loc, self.next_loc(), Token::Slash)),
                Some((loc, '.')) => return Some(spanned(loc, self.next_loc(), Token::Dot)),
                Some((_, '>')) if self.test_peek(|ch| ch == '=') => {
                    let (loc, _) = self.bump().unwrap();
                    return Some(spanned(loc, self.next_loc(), Token::GreaterEqual));
                }
                Some((loc, '>')) => return Some(spanned(loc, self.next_loc(), Token::Greater)),
                Some((_, '<')) if self.test_peek(|ch| ch == '=') => {
                    let (loc, _) = self.bump().unwrap();
                    return Some(spanned(loc, self.next_loc(), Token::LessEqual));
                }
                Some((loc, '<')) => return Some(spanned(loc, self.next_loc(), Token::Less)),
                Some((_, '=')) if self.test_peek(|ch| ch == '=') => {
                    let (loc, _) = self.bump().unwrap();
                    return Some(spanned(loc, self.next_loc(), Token::EqualEqual));
                }
                Some((loc, '=')) => return Some(spanned(loc, self.next_loc(), Token::Equal)),
                Some((_, '!')) if self.test_peek(|ch| ch == '=') => {
                    let (loc, _) = self.bump().unwrap();
                    return Some(spanned(loc, self.next_loc(), Token::NotEqual));
                }
                Some((loc, '!')) => return Some(spanned(loc, self.next_loc(), Token::Bang)),
                Some((loc, '`')) => return Some(spanned(loc, self.next_loc(), Token::BackTick)),
                Some((_, ':')) if self.test_peek(|ch| ch == '=') => {
                    let (loc, _) = self.bump().unwrap();
                    return Some(spanned(loc, self.next_loc(), Token::Assign));
                }
                Some((loc, ':')) => return Some(spanned(loc, self.next_loc(), Token::Colon)),
                Some((loc, ';')) => return Some(spanned(loc, self.next_loc(), Token::SemiColon)),
                Some((loc, ',')) => return Some(spanned(loc, self.next_loc(), Token::Comma)),
                Some((loc, '&')) => return Some(spanned(loc, self.next_loc(), Token::Amper)),
                Some((loc, '|')) => return Some(spanned(loc, self.next_loc(), Token::Vbar)),
                Some((start, '"')) => return Some(self.string_literal(start)),
                None => return None,
                _ => return None,
            }
        }
    }
}

fn error<T>(kind: ErrorKind, location: Location) -> Result<T, Error> {
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
            Some(error(ErrorKind::UnterminatedStringLiteral, start)),
            lexer.next()
        );
    }

    #[test]
    fn test_lex_unterminated_escapecode() {
        let mut lexer = Lexer::new("  \"hello, th\\");
        let start = Location::new(0, 12, 12);
        assert_eq!(Some(error(ErrorKind::UnexpectedEof, start)), lexer.next());
    }
}
