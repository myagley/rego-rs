use std::iter::Peekable;
use std::str::CharIndices;

use crate::token::Token;

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
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, usize, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}
