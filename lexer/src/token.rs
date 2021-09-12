

use log_derive::*;
use std::fmt::{Debug, Display};

use serde::{Deserialize, Serialize};

use strum::IntoEnumIterator;
use strum_macros::{Display as EnumDisplay, EnumIter};

#[derive(Deserialize, Serialize, Clone, Debug, PartialEq, PartialOrd, EnumDisplay)]
pub enum Token {
    Word(Word),
    
    Comment(String),
    
    /// (
    LParen,

    /// )
    RParen,

    /// ]
    RBracket,

    /// [
    LBracket,

    /// ,
    Comma,

    /// :
    Colon,

    /// .
    Dot,

    BinOp(BinOp),
    AssignOp(AssignOp),

    Char(String),
    Float(f64),
    Integer(i64),
    Boolean(bool),

    String(String),
}

impl Token {
    pub fn value(&self) -> String {
        match self {
            Token::Word(word) => match word {
                Word::Keyword(keyword) => keyword.to_string(),
                Word::Identifier(Identifier(value)) => value.to_string(),
            },
            Self::Dot => '.'.to_string(),
            Self::Comment(value) => value.to_string(),
            Self::AssignOp(value) => value.to_string(),
            Self::BinOp(value) => match value.to_string().chars().next().unwrap() {
                c if c.is_alphabetic() => ".".to_string() + value.to_string().as_str() + "",
                _ => value.to_string(),
            },
            Self::Boolean(value) => match value {
                true => ".true.".to_string(),
                false => ".false.".to_string(),
            },
            Self::Char(value) => value.to_string(),
            Self::Float (value) => value.to_string(),
            Self::Integer(value) => value.to_string(),
            Self::Colon => ':'.to_string(),
            Self::Comma => ','.to_string(),
            Self::LParen => '('.to_string(),
            Self::RParen => ')'.to_string(),
            Self::RBracket => ']'.to_string(),
            Self::LBracket => '['.to_string(),
            Self::String(s) => s.to_string()
        }
    }
}

// impl Display for Token {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Token::Word(word) => match word {
//                 Word::Keyword(keyword) => {
//                     write!(f, "{}", keyword.to_string())
//                 },
//                 Word::Identifier(Identifier(value)) => {
//                     write!(f, "{}", value)
//                 }
//             },
//             Self::Comment(value) => write!(f, "{}", value),
//             Self::AssignOp(value) => write!(f, "{}", value),
//             Self::BinOp(value) => write!(f, "{}", value),
//             Self::Boolean(value) => write!(f, "{}", value),
//             Self::Char(value) => write!(f, "{}", value),

//             Self::Colon(value) => write!(f, "{}", value),
//             Self::Comma(value) => write!(f, "{}", value),
//             Self::Float(value) => write!(f, "{}", value),
//             Self::Integer(value) => write!(f, "{}", value),
//             Self::LParen => write!(f, "("),
//             Self::RParen => write!(f, ")"),

//         }
//     }
// }

#[derive(Deserialize, Serialize, Clone, Debug, PartialEq, PartialOrd, EnumDisplay)]
pub enum TokenError {
    PossibleIdentifier,
    InvalidToken,
    PossibleToken,
    IncompleteComment,
}

impl Token {
    #[logfn(Info)]
    #[logfn_inputs(Info)]
    pub fn lex<Str: Display + Debug>(input: Str) -> Result<Self, TokenError> {
        let input = input.to_string();

        match input.as_str() {
            "(" => return Ok(Token::LParen),
            ")" => return Ok(Token::RParen),
            "," => return Ok(Token::Comma),
            ":" => return Ok(Token::Colon),
            ".true." => return Ok(Token::Boolean(true)),
            ".false." => return Ok(Token::Boolean(true)),
            _ => {}
        };

        for bin_op in BinOp::iter() {
            if bin_op.to_string().to_lowercase() == input.to_lowercase() {
                return Ok(Token::BinOp(bin_op));
            }
        }

        for assign_op in AssignOp::iter() {
            if assign_op.to_string().to_lowercase() == input.to_lowercase() {
                return Ok(Token::AssignOp(assign_op));
            }
        }

        match Word::from_string(input.clone()) {
            Ok(word) => return Ok(Token::Word(word)),
            Err(WordError::PossibleIdentifier) => return Err(TokenError::PossibleIdentifier),
            Err(WordError::NotAWord) => {}
        }

        if input.starts_with(".") && input.len() <= 5 {
            return Err(TokenError::PossibleToken);
        }

        if input.starts_with("$") {
            todo!("Add Comment Support");
        }

        Err(TokenError::InvalidToken)
    }
}

#[cfg(test)]
mod tokens {
    use crate::token::{BinOp, Identifier, Keyword, Token, TokenError, Word};

    #[tokio::test]
    async fn basic_tests() {
        let tokens = vec![
            (
                "a100version2",
                Ok(Token::Word(Word::Identifier(Identifier(
                    "a100version2".to_string(),
                )))),
            ),
            (
                "_a100_version2_",
                Ok(Token::Word(Word::Identifier(Identifier(
                    "_a100_version2_".to_string(),
                )))),
            ),
            (
                "_str_",
                Ok(Token::Word(Word::Identifier(Identifier(
                    "_str_".to_string(),
                )))),
            ),
            (
                "function",
                Ok(Token::Word(Word::Keyword(Keyword::Function))),
            ),
            (
                "function1",
                Ok(Token::Word(Word::Identifier(Identifier(
                    "function1".to_string(),
                )))),
            ),
            (".true.", Ok(Token::Boolean(true))),
            (".true", Err(TokenError::PossibleToken)),
            (".too_long_to_be_possible", Err(TokenError::InvalidToken)),
            (".gt.", Ok(Token::BinOp(BinOp::Gt))),
        ];

        for (token, expected) in tokens {
            assert_eq!(Token::lex(token), expected)
        }
    }

    #[tokio::test]
    async fn invalid_identifier() {}
}

#[derive(
    Deserialize,
    Serialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumDisplay,
    EnumIter,
)]
pub enum AssignOp {
    /// =
    #[strum(serialize = "=")]
    Assign,
}

#[derive(
    Deserialize,
    Serialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumDisplay,
    EnumIter,
)]
pub enum BinOp {
    /// .OR.
    #[strum(serialize = ".OR.")]
    Or,

    /// .AND.
    #[strum(serialize = ".AND.")]
    And,

    /// .NOT.
    #[strum(serialize = ".NOT.")]
    Not,

    /// .GT.
    #[strum(serialize = ".GT.")]
    Gt,

    /// .GE.
    #[strum(serialize = ".GE.")]
    GtEq,

    /// .LT.
    #[strum(serialize = ".LT.")]
    Lt,

    /// .LE.
    #[strum(serialize = ".LE.")]
    LtEq,

    /// .EQ.
    #[strum(serialize = ".EQ.")]
    Eq,

    /// .NE.
    #[strum(serialize = ".NE.")]
    NotEq,

    /// +
    #[strum(serialize = "+")]
    Plus,

    /// -
    #[strum(serialize = "-")]
    Minus,

    /// *
    #[strum(serialize = "*")]
    Mul,

    /// /
    #[strum(serialize = "/")]
    Div,

    /// **
    #[strum(serialize = "**")]
    Power,
}

impl BinOp {
    pub fn from_string(data: String) -> Option<Self> {
        let data = data.to_string();

        for bin_op in BinOp::iter() {
            if data == bin_op.to_string().to_lowercase() {
                return Some(bin_op);
            }
        }

        None
    }
}

#[derive(
    Deserialize, Serialize, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, EnumDisplay,
)]
pub enum Word {
    Keyword(Keyword),
    Identifier(Identifier),
}

#[derive(
    Deserialize,
    Serialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumDisplay,
    EnumIter,
)]
pub enum WordError {
    NotAWord,
    PossibleIdentifier,
}

impl Word {
    pub fn from_string(data: String) -> Result<Self, WordError> {
        let data = data.to_string();

        for keyword in Keyword::iter() {
            if data == keyword.to_string() {
                return Ok(Self::Keyword(keyword));
            }
        }

        let underscore_matches = data.matches("_").count();

        if data.starts_with("_") {
            if underscore_matches > 3 {
                return Err(WordError::NotAWord);
            }
            if underscore_matches <= 3 && (!data.starts_with("__") && !data.ends_with("__")) {
                if data.ends_with("_") {
                    return Ok(Self::Identifier(Identifier(data)));
                } else {
                    return Err(WordError::PossibleIdentifier);
                }
            }
            return Err(WordError::NotAWord);
        }

        let first = data.chars().next().unwrap();
        if first.is_alphabetic() && underscore_matches <= 1 && !data.ends_with("_") {
            return Ok(Self::Identifier(Identifier(data)));
        }
        Err(WordError::NotAWord)
    }
}

#[derive(Deserialize, Serialize, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(pub String);

impl From<String> for Identifier { 
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl From<&str> for Identifier { 
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

/// # Keywords
///
/// Just the keywords of the language
#[derive(
    Deserialize,
    Serialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumDisplay,
    EnumIter,
)]
pub enum Keyword {
    #[strum(serialize = "function")]
    Function,

    #[strum(serialize = "subroutine")]
    Subroutine,

    #[strum(serialize = "end")]
    End,

    #[strum(serialize = "integer")]
    Integer,

    #[strum(serialize = "real")]
    Real,

    #[strum(serialize = "logical")]
    Logical,

    #[strum(serialize = "character")]
    Character,
    
    #[strum(serialize = "string")]
    String,
    
    #[strum(serialize = "var")]
    Var,

    #[strum(serialize = "record")]
    Record,

    #[strum(serialize = "endrec")]
    Endrec,

    #[strum(serialize = "data")]
    Data,

    #[strum(serialize = "continue")]
    Continue,

    #[strum(serialize = "goto")]
    Goto,

    #[strum(serialize = "call")]
    Call,

    #[strum(serialize = "read")]
    Read,

    #[strum(serialize = "write")]
    Write,

    #[strum(serialize = "if")]
    If,

    #[strum(serialize = "then")]
    Then,

    #[strum(serialize = "else")]
    Else,

    #[strum(serialize = "endif")]
    Endif,

    #[strum(serialize = "do")]
    Do,

    #[strum(serialize = "enddo")]
    Enddo,

    #[strum(serialize = "stop")]
    Stop,

    #[strum(serialize = "return")]
    Return,
}

impl Keyword {
    fn into_word(self) -> Word {
        self.into()
    }
}

impl From<Keyword> for Word {
    fn from(kwd: Keyword) -> Self {
        Word::Keyword(kwd)
    }
}

impl From<Identifier> for Word {
    fn from(id: Identifier) -> Self {
        Word::Identifier(id)
    }
}
