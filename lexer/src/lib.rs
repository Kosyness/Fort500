use core::{num, panic};

use serde::{Deserialize, Serialize};
use std::{
    fmt::Debug,
    iter::{Enumerate, Peekable},
    str::Chars,
};

use log_derive::{logfn, logfn_inputs};

use log::{debug, info, trace, warn};
use token::{BinOp, Token, Word};

use crate::token::AssignOp;

pub mod token;
pub type LexerResult<T> = Result<Option<T>, TokenError>;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Ord, PartialOrd, Serialize, Deserialize)]
pub enum TokenError {
    EOF,
}

// pub trait Input : Debug + Clone {
//     fn next(&mut self) -> Option<char>;
//     fn peek(&mut self) -> Option<&char>;

//     fn take_white<P>(&mut self, predicate: P) -> String where
//         Self: Sized,
//         P: FnMut(&char) -> bool {
//             let mut string = "".to_string();
//             loop {
//                 match self.next() {
//                     Some(character) => if predicate(&character){
//                         string += character.to_string().as_str();
//                     }
//                 }
//             }
//         }
// }

#[derive(Clone, Debug)]
pub struct StringInput<'chars> {
    original: String,
    chars: Peekable<Chars<'chars>>,
    current_pos: usize,
}

// impl <'chars> Input for StringInput<'chars> {
//     fn next(&mut self) -> Option<char> {
//         let character = self.chars.next()?;
//         self.current_pos += 1;
//         Some(character)
//     }

//     fn peek(&mut self) -> Option<&char> {
//         let character = self.chars.peek()?;
//         self.current_pos += 1;
//         Some(character)
//     }
// }

#[derive(Debug, Clone)]
pub struct Lexer<'chars> {
    input: Peekable<Chars<'chars>>,
    original: String,
    current_pos: usize,

    input_len: usize,
    line: usize,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Ord, PartialOrd, Serialize, Deserialize)]
pub struct Span {
    #[serde(rename = "start")]
    pub lo: usize,
    #[serde(rename = "end")]
    pub hi: usize,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TokenSpan {
    pub token: Token,
    pub span: Span,
}

impl<'chars> Lexer<'chars> {
    pub fn new(data: Peekable<Chars<'chars>>) -> Self {
        Self {
            input: data.clone(),
            current_pos: 0,
            original: data.clone().collect::<String>(),
            input_len: { data.clone().collect::<String>() }.len(),
            line: 0,
        }
    }

    pub fn lex(&mut self) -> Result<Vec<TokenSpan>, TokenError> { 
        let mut tokens = vec![];

        while let Ok(token_span) = self.next_token() { 
            tokens.push(token_span)
        }

        Ok(tokens)
    }

    pub fn next_token(&mut self) -> Result<TokenSpan, TokenError> {
        match self.read_token() {
            Ok(Some(token)) => Ok(TokenSpan {
                token: token.clone(),
                span: {
                    let new_len: usize = {
                        let temp: String = self.input.clone().collect();
                        temp.len()
                    };
                    let token_len = token.value().len();

                    self.current_pos = self.input_len - new_len;
                    let current_len = self.current_pos;
                    Span {
                        lo: current_len - token_len,
                        hi: current_len,
                    }
                },
            }),
            Ok(None) => Err(TokenError::EOF),
            Err(err) => Err(err),
        }
    }

    #[logfn(Trace)]
    pub fn read_token(&mut self) -> LexerResult<Token> {
        let character = match self.input.peek() {
            Some(c) => *c,
            None => return Err(TokenError::EOF),
        };

        match character {
            ' ' | '\n' | '\t' => return self.handle_whitespace(),
            '(' | ')' | ',' | ':' => return self.hadle_special(),
            '.' => return self.handle_dot(),
            '/' | '*' | '+' | '-' | '=' => return self.handle_operators(),
            '$' => return self.handle_comment(),
            '0' => return self.handle_radix_number(),
            '1'..='9' => return self.handle_number(),
            '"' => return self.handle_string(),
            c if c.is_ident_start() => return self.handle_ident_or_keyword(),
            '\'' => return self.handle_character(),
            character => panic!("Invalid Character '{}'", character),
        };
    }

    fn handle_string(&mut self) -> LexerResult<Token> { 
        self.input.next();
        let mut escaped = false;
        let s = self.read_until_fn(|c| { 
            match c { 
                '"' if !escaped => {
                    false
                }
                _ if !escaped => { 
                    true
                }
                c if escaped => { 
                    escaped = false;
                    todo!("handle escaped character \\{}", c);
                }
                '\\' => {
                    escaped = true;
                    todo!("handle backslash");
                    true
                },
                _ => unreachable!()
            }
        });
        
        self.input.next();
        Ok(Some(Token::String(s)))
    }

    fn handle_e_number(&mut self, input: String) -> LexerResult<Token> {
        self.input.next();
        #[derive(Debug)]
        enum Type {
            Positive,
            Negative,
        }
        let mut epsilon = "".to_string();

        let mut type_ = None;

        'to_break: loop {
            match self.input.next() {
                Some(c) if c == '+' && type_.is_none() => {
                    type_ = Some(Type::Positive);
                }
                Some(c) if c == '-' && type_.is_none() => {
                    type_ = Some(Type::Negative);
                }
                Some(c) if c == '-' || c == '+' => {
                    panic!("Unexpected Token found {}", c);
                }
                Some(c) if c.is_numeric() => {
                    println!("FOUND NUMERIC {}", c);
                    epsilon += c.to_string().as_str();
                }
                Some(c) => {
                    if !epsilon.is_empty() {
                        break 'to_break;
                    }
                    match type_ {
                        Some(_) => panic!("Expected a number after E and the sign, found {}", c),
                        None => panic!(
                            "Expected either a Sign or a number after the E, found {}",
                            c
                        ),
                    }
                }
                None => {
                    break 'to_break;
                }
            }
        }
        if type_.is_none() {
            type_ = Some(Type::Positive);
        }
        println!(
            "Converting Scientific Notation Number {} e {:?} {}",
            input, type_, epsilon
        );
        Ok(Some(Token::Integer(69)))
    }

    fn handle_number(&mut self) -> LexerResult<Token> {
        let mut contains_dot = false;
        let mut handle_e = false;
        let number_str = self.read_until_fn(|character| match character {
            '0'..='9' => true,
            '.' if !contains_dot => {
                contains_dot = true;
                true
            }
            'E' | 'e' => {
                handle_e = true;
                false
            }
            _ => false,
        });

        if handle_e {
            return self.handle_e_number(number_str);
        }

        let (mut has_first, mut has_second) = (true, true);
        let (first, second) = {
            if contains_dot {
                let mut values = number_str.split(".").collect::<Vec<_>>();
                if values[0].to_string().is_empty() {
                    has_first = false;
                    values[0] = "0";
                }
                (
                    values[0].to_string(),
                    {
                        if values.len() >= 2 {
                            values[1].to_string()
                        } else {
                            "".to_string()
                        }
                    }
                    .to_string(),
                )
            } else {
                has_second = false;
                (number_str, "0".to_string())
            }
        };

        Ok(Some(match contains_dot {
            true => Token::Float(format!("{}.{}", first, second).parse().unwrap()),
            false => Token::Integer(i64::from_str_radix(first.as_str(), 10).unwrap()),
        }))
    }

    fn handle_radix_number(&mut self) -> LexerResult<Token> {
        self.input.next();
        if self.input.peek().is_none() {
            return Ok(Some(Token::Integer(0)));
        }
        let radix = match self.input.next().unwrap() {
            'h' | 'H' => 16,
            'b' | 'B' => 2,
            _ => match self.input.peek() {
                Some(c) if c.is_numeric() => return self.handle_number(),
                _ => return Ok(Some(Token::Integer(0))),
            }, // _ => panic!("Binary and Hex Numbers are in the form of 0H/0h and 0B/0b. Missing B/H")
        };

        let mut contains_dot = false;
        let number_str = match radix {
            16 => self.read_until_fn(move |character| match character {
                '.' if !contains_dot => {
                    contains_dot = true;
                    true
                }
                '0'..='9' | 'a'..='f' | 'A'..='F' => true,
                _ => false,
            }),
            2 => self.read_until_fn(move |character| match character {
                '.' if !contains_dot => {
                    contains_dot = true;
                    true
                }
                '0' | '1' => true,
                _ => false,
            }),
            _ => unreachable!("How did you even.."),
        };
        let contains_dot = contains_dot | number_str.starts_with(".");

        let (mut has_first, mut has_second) = (true, true);
        let (first, second) = {
            if contains_dot {
                let mut values = number_str.split(".").collect::<Vec<_>>();
                if values[0].to_string().is_empty() {
                    has_first = false;
                    values[0] = "0";
                }
                (
                    values[0].to_string(),
                    {
                        if values.len() >= 2 {
                            values[1].to_string()
                        } else {
                            "".to_string()
                        }
                    }
                    .to_string(),
                )
            } else {
                has_second = false;
                (number_str, "0".to_string())
            }
        };

        println!("{} {}", first, second);

        
        Ok(Some(match contains_dot {
            true => Token::Float(i64::from_str_radix(first.as_str(), radix).unwrap() as f64),
            false => Token::Integer(i64::from_str_radix(first.as_str(), radix).unwrap()),
        }))
    }

    fn hadle_special(&mut self) -> LexerResult<Token> {
        Ok(Some(match self.input.next().unwrap() {
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,
            ':' => Token::Colon,
            // '=' => Token::AssignOp(AssignOp::Assign),
            _ => unreachable!("This should only be called with special as the next value"),
        }))
    }

    fn handle_character(&mut self) -> LexerResult<Token> {
        self.input.next();
        let character = "".to_string() + self.read_until('\'').as_str();
        let special = match self.input.next() {
            Some(v) => match v {
                't' | 'f' | 'r' | 'v' => {
                    self.input.next();
                    v.to_string()
                }
                _ => "".to_string(),
            },
            None => "".to_string(),
        };
        let character = character + special.as_str();

        // let character = character.chars().next().unwrap();
        Ok(Some(Token::Char(character)))
    }

    fn handle_comment(&mut self) -> LexerResult<Token> {
        self.read_until('\n');
        self.read_token()
    }

    #[logfn(Trace)]
    fn handle_operators(&mut self) -> LexerResult<Token> {
        Ok(Some(match self.input.next().unwrap() {
            '-' => Token::BinOp(BinOp::Minus),
            '+' => Token::BinOp(BinOp::Plus),
            '/' => Token::BinOp(BinOp::Div),
            '*' => match self.input.peek() {
                Some('*') => {
                    self.input.next();
                    Token::BinOp(BinOp::Power)
                }
                _ => Token::BinOp(BinOp::Mul),
            },
            '=' => Token::AssignOp(AssignOp::Assign),
            c => unreachable!(
                "This should only be called with operators as the next value (found '{}')",
                c
            ),
        }))
    }

    fn handle_dotted_number(&mut self) -> LexerResult<Token> {
        Ok(Some(Token::Float(1f64)))
        // todo!("Add Support for Float Numbers")
    }

    #[logfn(Trace)]
    fn handle_dot(&mut self) -> LexerResult<Token> {
        let data = self.input.next().unwrap().to_string();

        match self.input.peek() {
            Some('0'..='9') => return self.handle_dotted_number(),
            _ => {}
        }

        let data = data + self.read_until('.').as_str() + ".";
        self.input.next();
        debug!("Checking for dot value {}", data);

        if let Some(bin_op) = BinOp::from_string(data.clone()) {
            return Ok(Some(Token::BinOp(bin_op)));
        } else if data == ".true." {
            return Ok(Some(Token::Boolean(true)));
        } else if data == ".false." {
            return Ok(Some(Token::Boolean(false)));
        }
        panic!("Expected a binary operation or boolean");
    }

    #[logfn(Debug)]
    fn handle_whitespace(&mut self) -> LexerResult<Token> {
        debug!(target: "handle_whitespace", "Handling Whitespace");
        self.read_until_not_whitespace();
        self.read_token()
    }

    #[logfn(Trace)]
    fn handle_ident_or_keyword(&mut self) -> LexerResult<Token> {
        let input = self.read_until_fn(|character| match character {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => true,
            _ => false,
        });

        let token = Word::from_string(input);

        match token {
            Ok(token) => {
                return Ok(Some(Token::Word(token)));
            }
            Err(e) => panic!("error: {:?}", e),
        }
    }

    fn read_until_fn<Func: FnMut(char) -> bool>(&mut self, mut func: Func) -> String {
        let data = self.input.clone().take_while(|&value| func(value));

        let result: String = data.collect();
        for _ in 0..result.len() {
            self.input.next();
        }

        result
    }

    #[logfn(Trace)]
    fn read_until(&mut self, character: char) -> String {
        let data = self.input.clone().take_while(|&value| {
            trace!(target: "lexer::read_until", "Checking current '{}' with '{}'", value, character);
            value != character
        });
        let result: String = data.collect();

        for _ in 0..result.len() {
            self.input.next();
        }

        debug!("Read Until '{}', with result '{}'", character, result);

        result
    }

    fn read_until_not_whitespace(&mut self) -> String {
        let data = self.input.clone().take_while(|&value| match value {
            ' ' | '\t' => true,
            '\n' => {
                self.line += 1;
                true
            }
            _ => false,
        });

        let result: String = data.collect();
        for _ in 0..result.len() {
            self.input.next();
        }

        result
    }

    #[logfn(Trace)]
    fn read_until_with_count(&mut self, character: char, count: usize) -> String {
        let mut found = 0;
        let data = self.input.by_ref().take_while(|&value| {
            trace!(target: "lexer::read_until", "Checking current '{}' with '{}'. {} found, wanting {}", value, character, found, count);

                if value == character {
                found += 1;
            }

            found < count
        });
        let result = data.collect();

        debug!("Read Until '{}', with result '{}'", character, result);

        result
    }

    #[logfn(Trace)]
    fn read_until_not(&mut self, character: char) -> String {
        let data = self.input.clone().take_while(|&value| value == character);

        let result = data.collect::<String>();
        for _ in 0..result.len() {
            self.input.next();
        }

        debug!(target: "lexer::read_until", "Read {} characters", result.len() as usize);

        result
    }
}

#[cfg(test)]
mod lexer_tests {
    use crate::Lexer;

    #[tokio::test]
    async fn lexer() {
        env_logger::init();
        let input = r#"
"hello world"
"#
        .to_string();
        let iterator = input.chars();

        let mut lexer = Lexer::new(iterator.peekable());

        let s = serde_json::to_string(&lexer.lex()).unwrap();
        println!("{}", s);
    }
}

trait IsIdentifierStart {
    fn is_ident_start(&self) -> bool;
}

impl IsIdentifierStart for char {
    fn is_ident_start(&self) -> bool {
        self.is_alphabetic() || self == &'_'
    }
}
