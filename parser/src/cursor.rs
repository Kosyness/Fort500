use lexer::{TokenSpan, token::{Identifier, Token, Word}};

use crate::{ParseError, ParseResult};


#[derive(Debug, Clone)]
pub struct Cursor {
    tokens: Vec<TokenSpan>,
    current_token: usize,
}

impl Cursor {
    pub fn new(tokens: Vec<TokenSpan>) -> Self {
        Self {
            tokens,
            current_token: 0,
        }
    }

    pub fn next(&mut self) -> Option<TokenSpan> {
        self.current_token += 1;
        if self.tokens.len() > self.current_token - 1 {
            Some(self.tokens[self.current_token - 1].clone())
        } else {
            None
        }
    }

    pub fn peek(&self) -> Option<TokenSpan> {
        if self.tokens.len() > self.current_token {
            Some(self.tokens[self.current_token].clone())
        } else {
            None
        }
    }

    pub fn expect_fn<F: Fn(Token) -> bool>(&mut self, expect: F) -> ParseResult<TokenSpan> {
        if let Some(TokenSpan { token, span }) = self.peek() {
            if expect(token.clone()) {
                Ok(self.next().unwrap())
            } else {
                Err(ParseError::Expected(
                    "Unexpected token".to_string(),
                    vec![],
                    self.peek(),
                ))
            }
        } else {
            Err(ParseError::Expected(
                "Unexpected token".to_string(),
                vec![],
                self.peek(),
            ))
        }
    }

    pub fn expect(&mut self, expect: Token) -> ParseResult<TokenSpan> {
        if let Some(TokenSpan { token, span }) = self.peek() {
            if expect == token {
                Ok(self.next().unwrap())
            } else {
                Err(ParseError::Expected(
                    "Unexpected token".to_string(),
                    vec![expect],
                    self.peek(),
                ))
            }
        } else {
            Err(ParseError::Expected(
                "Unexpected token".to_string(),
                vec![expect],
                self.peek(),
            ))
        }
    }

    pub fn expect_ident(&mut self) -> ParseResult<TokenSpan> {
        if let Some(TokenSpan { token, span }) = self.peek() {
            if let Token::Word(Word::Identifier(_)) = token {
                Ok(TokenSpan { token, span })
            } else {
                Err(ParseError::Expected(
                    "Unexpected token".to_string(),
                    vec![Token::Word(Word::Identifier(Identifier(
                        "Identifier".to_string(),
                    )))],
                    self.peek(),
                ))
            }
        } else {
            Err(ParseError::Expected(
                "Unexpected token".to_string(),
                vec![Token::Word(Word::Identifier(Identifier(
                    "Identifier".to_string(),
                )))],
                self.peek(),
            ))
        }
    }
}
