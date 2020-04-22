use std::iter::Peekable;
use std::str::CharIndices;

use crate::token::{StringLiteral, Token};

pub enum Error {
    UnterminatedStringLiteral,
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct Lexer<'input> {
    chars: Peekable<CharIndices<'input>>,
    input: &'input str,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            chars: input.char_indices().peekable(),
            input,
        }
    }

    fn test_lookahead<F>(&mut self, mut test: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        self.chars.peek().map_or(false, |(_, ch)| test(*ch))
    }

    fn escape_code(&mut self) -> Result<(), ()> {
        self.chars.next();
        Ok(())
    }

    fn string_literal(&mut self, start: usize) -> Spanned<Token<'input>, usize, ()> {
        let content_start = start + 1;
        loop {
            match self.chars.next() {
                Some((_, '\\')) => {
                    self.escape_code()?;
                }
                Some((i, '"')) => {
                    let s = StringLiteral::Escaped(&self.input[content_start..i]);
                    return Ok((start, Token::StringLiteral(s), i + 1));
                }
                Some((_i, _)) => continue,
                None => break,
            }
        }
        panic!("afadf");
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, usize, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.chars.next() {
                Some((i, '\n')) => return Some(Ok((i, Token::Newline, i + 1))),
                Some((i, '\r')) => return Some(Ok((i, Token::CarriageReturn, i + 1))),
                Some((_, ch)) if ch.is_whitespace() => continue,
                Some((i, '{')) => return Some(Ok((i, Token::BraceOpen, i + 1))),
                Some((i, '}')) => return Some(Ok((i, Token::BraceClose, i + 1))),
                Some((i, '[')) => return Some(Ok((i, Token::BracketOpen, i + 1))),
                Some((i, ']')) => return Some(Ok((i, Token::BracketClose, i + 1))),
                Some((i, '(')) => return Some(Ok((i, Token::ParenOpen, i + 1))),
                Some((i, ')')) => return Some(Ok((i, Token::ParenClose, i + 1))),
                Some((i, '+')) => return Some(Ok((i, Token::Plus, i + 1))),
                Some((i, '-')) => return Some(Ok((i, Token::Minus, i + 1))),
                Some((i, '*')) => return Some(Ok((i, Token::Star, i + 1))),
                Some((i, '/')) => return Some(Ok((i, Token::Slash, i + 1))),
                Some((i, '.')) => return Some(Ok((i, Token::Dot, i + 1))),
                Some((i, '>')) if self.test_lookahead(|ch| ch == '=') => {
                    self.chars.next();
                    return Some(Ok((i, Token::GreaterEqual, i + 2)));
                }
                Some((i, '>')) => return Some(Ok((i, Token::Greater, i + 1))),
                Some((i, '<')) if self.test_lookahead(|ch| ch == '=') => {
                    self.chars.next();
                    return Some(Ok((i, Token::LessEqual, i + 2)));
                }
                Some((i, '<')) => return Some(Ok((i, Token::Less, i + 1))),
                Some((i, '=')) if self.test_lookahead(|ch| ch == '=') => {
                    self.chars.next();
                    return Some(Ok((i, Token::EqualEqual, i + 2)));
                }
                Some((i, '=')) => return Some(Ok((i, Token::Equal, i + 1))),
                Some((i, '!')) if self.test_lookahead(|ch| ch == '=') => {
                    self.chars.next();
                    return Some(Ok((i, Token::NotEqual, i + 2)));
                }
                Some((i, '!')) => return Some(Ok((i, Token::Bang, i + 1))),
                Some((i, '`')) => return Some(Ok((i, Token::BackTick, i + 1))),
                Some((i, ':')) if self.test_lookahead(|ch| ch == '=') => {
                    self.chars.next();
                    return Some(Ok((i, Token::Assign, i + 2)));
                }
                Some((i, ':')) => return Some(Ok((i, Token::Colon, i + 1))),
                Some((i, ';')) => return Some(Ok((i, Token::SemiColon, i + 1))),
                Some((i, ',')) => return Some(Ok((i, Token::Comma, i + 1))),
                Some((i, '&')) => return Some(Ok((i, Token::Amper, i + 1))),
                Some((i, '|')) => return Some(Ok((i, Token::Vbar, i + 1))),
                Some((i, '"')) => return Some(self.string_literal(i)),
                None => return None,
                _ => return None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_string() {
        let mut lexer = Lexer::new("  \"hello, there\" ");
        assert_eq!(
            Some(Ok((
                2,
                Token::StringLiteral(StringLiteral::Escaped("hello, there")),
                16
            ))),
            lexer.next()
        );
    }
}
