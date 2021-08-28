use colored::*;
use core::panic;
use hex::ToHex;
use std::{
    fmt::Display,
    hash::Hash,
    iter::{Enumerate, Peekable},
    slice::Iter,
    vec,
};

use serde::{de::Expected, Deserialize, Serialize};

use lexer::{
    token::{Identifier, Keyword, Token, Word},
    Lexer, TokenSpan,
};

use pad::PadStr;
use std::fmt::Write;

#[derive(Clone, Debug)]
pub struct Parser {
    tokens: Vec<TokenSpan>,
    current_token: usize,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    DeclarationStmt {
        variable_type: String,
        symbol: Identifier,
        initial_value: Option<Box<Expr>>,
    },
    DimensionStmt(Box<Expr>),
}

use strum_macros::{Display as EnumDisplay, EnumIter};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, EnumDisplay)]
#[serde(tag = "type", content = "data")]
pub enum Expr {
    EmptyExpr,
    /// Program -> body(BodyExpr) END subprogram(SubprogramsExpr)
    ProgramExpr(Box<Expr>, Vec<Box<Expr>>),

    /// body -> declarations statements
    ///
    /// TODO
    BodyExpr(Box<Expr>, Box<Expr>),

    /// declarations -> declarations(This) declaration(DeclarationExpr)
    ///
    /// Holds: Vec DeclarationExpr
    DeclarationsExpr(Vec<Box<Expr>>),

    /// declaration ->
    ///     - declaration_record
    ///     - declaration_data
    ///     - declaration_type
    ///     - (empty)
    ///
    /// Holds: One of: DeclarationExprData, DeclarationExprRecord, DeclarationExprType
    DeclarationExpr(Box<Expr>),

    /// declaration_record -> RECORD fields ENDREC vars
    ///
    /// Holds: FieldsExpr, VarsExpr
    DeclarationExprRecord(Box<Expr>, Box<Expr>),

    /// declaration_data -> DATA vals
    ///
    /// TODO
    DeclarationExprData(),

    /// declaration_type -> type vars
    ///
    /// Holds: TypeExpr, VarsExpr
    DeclarationExprType(Box<Expr>, Box<Expr>),

    /// TODO
    SubProgramsExpr(Box<Expr>, Box<Expr>),

    /// vars ->
    ///     - vars(This) COMMA undef_variable(VarDeclaration)
    ///     - undef_variable(VarDeclaration)
    ///
    /// Holds: Vec of VarDeclarationExpr
    VarsExpr(Vec<Box<Expr>>),

    /// undef_variable ->
    ///     - ID
    ///     - ID LPAREN dims(DimensionsExpr) RPAREN
    ///
    /// Holds: Identifier, DimensionsExpr(None if no dimensions)
    VarDeclarationExpr(Box<Expr>, Option<Box<Expr>>),

    /// dims ->
    ///     - dims(This) COMMA dim
    ///     - dim
    ///
    /// Holds: Vec<DimensionExpr>
    DimensionsExpr(Vec<Box<Expr>>),

    /// dim ->
    ///     - ICONST(ConstExpr)
    ///     - ID(Identifier)
    ///
    /// Holds: Either Const or Identifier
    DimensionExpr(Box<Expr>),

    /// type -> INTEGER | CHAR | REAL | LOGICAL
    ///
    /// Holds: Keyword
    TypeExpr(Box<Expr>),

    /// fields ->
    ///     - fields(This) field(FieldExpr)
    ///     - field(FieldExpr)
    ///
    /// Holds: Vec of FieldExpr
    FieldsExpr(Vec<Box<Expr>>),

    /// field ->
    ///     - field_type(FieldExprType)
    ///     - field_record(FieldExprRecord)
    ///
    /// Holds: Either FieldExprType or FieldExprRecord
    FieldExpr(Box<Expr>),

    /// field_type -> type vars
    ///
    /// Holds: TypeExpr, VarsExpr
    FieldExprType(Box<Expr>, Box<Expr>),

    /// field_record -> RECORD fields ENDREC vars
    ///
    /// Holds: FieldsExpr, VarsExpr
    FieldExprRecord(Box<Expr>, Box<Expr>),

    /// vals ->
    ///     - vals(This) COMMA val(ValExpr)
    ///     - val(ValExpr)
    ///
    /// HOlds: Vec of ValExpr
    ValsExpr(Vec<Box<Expr>>),

    /// val -> ID value_list(See ValuesExpr)
    ///
    /// Holds Identifier, ValuesExpr
    ValExpr(Box<Expr>, Box<Expr>),

    /// value_list -> DIVOP values(This) DIVOP
    ///
    /// values ->
    ///     - values(This) COMMA value(ValueExpr)
    ///     - value(ValueExpr)
    ///
    /// Holds: Vec of ValueExpr
    ValuesExpr(Vec<Box<Expr>>),

    ValueExpr(),

    /// repeat -> ICONST | Empty
    RepeatExpr(Box<Expr>),

    /// Just a Keyword
    Keyword(Token),

    /// Just an Identifier
    Identifier(Token),

    /// Just a Constant
    Constant(Token),
}

#[test]
fn hello() {
    let input = r#"integer _hello_, _one_, _two_(_hello_, 1)
    real _real_hello_, _real_one_, _real_two_(_real_hello2_, 1)"#
    .to_string();

    let mut lexer = Lexer::new(input.chars().peekable());
    let tokens = lexer.lex().unwrap();

    let mut parser = Parser::new(tokens);

    let e = parser.declarations().unwrap();

    println!("{}", serde_json::to_string(&e).unwrap());
    // e.beautify();
}

use ptree::{print_tree, TreeBuilder};
impl Expr {
    pub fn beautify(&self) {
        let mut tree = TreeBuilder::new(self.pretty_name());
        self.add_to_tree(&mut tree);

        print_tree(&tree.build()).unwrap();
    }

    fn pretty_name(&self) -> String {
        let string = match self {
            // Expr::VarsExpr(list)
            // | Expr::DimensionsExpr(list)
            // | Expr::ValsExpr(list)
            // | Expr::ValuesExpr(list) | Expr::DimensionsExpr(list) => format!(
            //     "{} ({})",
            //     "DimensionsExpr".yellow().bold(),
            //     list.len().to_string().yellow()
            // )
            // .bright_black(),
            Expr::Identifier(id) => format!(
                "{}: {}",
                "Identifier".blue().bold(),
                id.value().green().bold()
            )
            .bright_black(),
            Expr::Keyword(keyword) => format!(
                "{}: {}",
                "Keyword".blue().bold(),
                keyword.value().green().bold()
            )
            .bright_black(),
            Expr::Constant(token) => format!(
                "{}: {} (type: {})",
                "Constant".blue().bold(),
                token.value().magenta().bold(),
                token.to_string().green().bold()
            )
            .bright_black(),
            e => e.to_string().yellow().bold(),
        }
        .to_string();
        String::from(format!("{}", string))
    }

    fn add_to_tree(&self, tree: &mut TreeBuilder) {
        match self {
            Expr::EmptyExpr => {}
            Expr::Constant(_) => {}
            Expr::DeclarationExpr(expr) => {
                tree.begin_child(expr.pretty_name());
                expr.add_to_tree(tree);
                tree.end_child();
            }
            Expr::VarsExpr(list)
            | Expr::DimensionsExpr(list)
            | Expr::ValsExpr(list)
            | Expr::ValuesExpr(list)
            | Expr::DeclarationsExpr(list) => {
                for (index, l) in list.iter().enumerate() {
                    tree.begin_child(
                        format!(
                            "{} [{}]",
                            l.pretty_name(),
                            index.to_string().bright_blue().bold()
                        )
                        .bright_black()
                        .to_string(),
                    );
                    l.add_to_tree(tree);
                    tree.end_child();
                }
            }
            Expr::DimensionExpr(first) => {
                tree.begin_child(first.pretty_name());
                first.add_to_tree(tree);
                tree.end_child();
            }
            Expr::FieldExpr(first) => {
                tree.begin_child(first.pretty_name());
                first.add_to_tree(tree);
                tree.end_child();
            }
            Expr::FieldsExpr(list) => {
                for l in list {
                    tree.begin_child(l.pretty_name());
                    l.add_to_tree(tree);
                    tree.end_child();
                }
            }
            Expr::Identifier(token) | Expr::Keyword(token) => {}
            Expr::TypeExpr(first) => {
                tree.begin_child(first.pretty_name());
                first.add_to_tree(tree);
                tree.end_child();
            }

            Expr::VarDeclarationExpr(first, second) => {
                tree.begin_child(first.pretty_name());
                first.add_to_tree(tree);
                tree.end_child();

                if let Some(second) = second {
                    tree.begin_child(second.pretty_name());
                    second.add_to_tree(tree);
                    tree.end_child();
                }
            }
            Expr::BodyExpr(first, second)
            | Expr::DeclarationExprRecord(first, second)
            | Expr::DeclarationExprType(first, second)
            | Expr::FieldExprRecord(first, second)
            | Expr::ValExpr(first, second)
            | Expr::FieldExprType(first, second) => {
                tree.begin_child(first.pretty_name());
                first.add_to_tree(tree);
                tree.end_child();

                tree.begin_child(second.pretty_name());
                second.add_to_tree(tree);
                tree.end_child();
            }
            e => todo!("Missing add_to_tree for {}", e.to_string()),
        }
    }

    fn beautify_with_index(&self, tab: usize) -> String {
        let mut output = String::new();
        for _ in 0..tab - 1 {
            // println!("{}", "Hi there!".pad_to_width_with_alignment(16, pad::Alignment::Right));
            write!(&mut output, "  |  ").unwrap();
        }
        write!(&mut output, "  ∟  ").unwrap();

        output += "Hello";
        output
    }
}

type ParserResult<T> = Result<T, ParserError>;

#[derive(Clone, Debug)]
enum ParserError {
    InvalidState,
    Expected(String, Vec<Token>, Option<TokenSpan>),
    EOF,
}

impl Parser {
    fn new(tokens: Vec<TokenSpan>) -> Self {
        // todo!();
        Self {
            tokens,
            current_token: 0,
        }
    }

    fn ast(&mut self) -> Expr {
        let program = self.program();
        todo!();
        // loop {
        //     match self.tokens.peek() {
        //         Some((index, TokenSpan { span, token })) => match token {
        //             Token::Word(Word::Keyword(lexer::token::Keyword::Integer))
        //             | Token::Word(Word::Keyword(lexer::token::Keyword::Real))
        //             | Token::Word(Word::Keyword(lexer::token::Keyword::Logical))
        //             | Token::Word(Word::Keyword(lexer::token::Keyword::Character)) => {

        //             }
        //         },
        //         None => {}
        //     }
        // }
    }

    fn program(&mut self) -> ParserResult<Expr> {
        let body = self.body();

        todo!()
    }

    fn subprograms(&mut self) -> ParserResult<Vec<Box<Expr>>> {
        todo!()
    }

    fn body(&mut self) -> ParserResult<Box<Expr>> {
        // let declarations = self.declarations();
        todo!()
    }

    fn declarations(&mut self) -> ParserResult<Expr> {
        let mut declarations = vec![];

        while let Ok(declaration) = self.declaration() {
            declarations.push(Box::new(Expr::DeclarationExpr(Box::new(declaration))));
        }

        Ok(Expr::DeclarationsExpr(declarations))
    }

    fn declaration(&mut self) -> ParserResult<Expr> {
        Ok(match self.peek_token() {
            Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Record)),
                span: _,
            }) => {
                self.next_token();

                let fields = self.fields()?;

                match self.expect(Token::Word(Word::Keyword(Keyword::Endrec))) {
                    Some(_) => {}
                    None => {
                        return Err(ParserError::Expected(
                            "Expected Endrec after record".to_string(),
                            vec![Token::Word(Word::Keyword(Keyword::Endrec))],
                            self.peek_token(),
                        ))
                    }
                };

                let vars = self.vars()?;

                Expr::DeclarationExprRecord(Box::new(fields), Box::new(vars))
            }
            Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Data)),
                span: _,
            }) => {
                todo!();
            }
            Some(TokenSpan { token, span: _ }) => {
                // TODO: Should return empty if type_expr fails
                let type_expr = self.type_expr()?;

                // TODO: Should completely fail if vars_expr fails
                let vars_expr = self.vars()?;
                Expr::DeclarationExprType(Box::new(type_expr), Box::new(vars_expr))
            }
            _ => return Err(ParserError::InvalidState),
        })
    }

    fn fields(&mut self) -> ParserResult<Expr> {
        let mut fields = vec![];

        while let Ok(field) = self.field() {
            fields.push(Box::new(Expr::FieldExpr(Box::new(field))))
        }

        Ok(Expr::FieldsExpr(fields))
    }

    fn field(&mut self) -> ParserResult<Expr> {
        Ok(match self.peek_token() {
            Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Record)),
                span: _,
            }) => {
                self.next_token();
                let fields = self.fields()?;

                match self.expect(Token::Word(Word::Keyword(Keyword::Endrec))) {
                    Some(_) => {}
                    None => {
                        return Err(ParserError::Expected(
                            "Expected endrec after recording".to_string(),
                            vec![Token::Word(Word::Keyword(Keyword::Endrec))],
                            self.peek_token(),
                        ))
                    }
                };

                let vars = self.vars()?;

                Expr::FieldExprRecord(Box::new(fields), Box::new(vars))
            }
            Some(_) => {
                let type_expr = self.type_expr()?;

                let vars = self.vars()?;

                Expr::FieldExprType(Box::new(type_expr), Box::new(vars))
            }
            None => {
                return Err(ParserError::Expected(
                    "Expected either a Type or Record".to_string(),
                    vec![
                        Token::Word(Word::Keyword(Keyword::Integer)),
                        Token::Word(Word::Keyword(Keyword::Real)),
                        Token::Word(Word::Keyword(Keyword::Logical)),
                        Token::Word(Word::Keyword(Keyword::Character)),
                        Token::Word(Word::Keyword(Keyword::Record)),
                    ],
                    None,
                ))
            }
        })
    }

    fn type_expr(&mut self) -> ParserResult<Expr> {
        match self.peek_token() {
            Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Integer)),
                span: _,
            })
            | Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Real)),
                span: _,
            })
            | Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Character)),
                span: _,
            })
            | Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Logical)),
                span: _,
            }) => {
                let token = self.next_token().unwrap().token;
                return Ok(Expr::TypeExpr(Box::new(Expr::Keyword(token))));
            }
            token => {
                return Err(ParserError::Expected(
                    "Expected Type".to_string(),
                    vec![
                        Token::Word(Word::Keyword(Keyword::Integer)),
                        Token::Word(Word::Keyword(Keyword::Real)),
                        Token::Word(Word::Keyword(Keyword::Character)),
                        Token::Word(Word::Keyword(Keyword::Logical)),
                    ],
                    token,
                ))
            }
        };
    }

    fn vars(&mut self) -> ParserResult<Expr> {
        let mut variables = vec![];

        while let variable = self.undef_variable()? {
            variables.push(variable);

            match self.peek_token() {
                Some(TokenSpan {
                    token: Token::Comma,
                    span: _,
                }) => {
                    self.next_token();
                    continue;
                }
                _ => break,
            }
        }

        let variables = variables.iter().map(|e| Box::new(e.clone())).collect();
        Ok(Expr::VarsExpr(variables))
    }

    fn undef_variable(&mut self) -> ParserResult<Expr> {
        let identifier = match self.next_token() {
            Some(TokenSpan {
                token: Token::Word(Word::Identifier(id)),
                span,
            }) => TokenSpan {
                token: Token::Word(Word::Identifier(id)),
                span,
            },
            _ => {
                return Err(ParserError::Expected(
                    "Expected Identifier. Cannot define a variable, without a name".to_string(),
                    vec![Token::Word(Word::Identifier(Identifier("".to_string())))],
                    self.peek_token(),
                ))
            }
        };

        let _is_array = match self.peek_token() {
            Some(TokenSpan {
                token: Token::LParen,
                span: _,
            }) => true,
            _ => {
                return Ok(Expr::VarDeclarationExpr(
                    Box::new(Expr::Identifier(identifier.token)),
                    None,
                ))
            }
        };

        self.next_token();
        let dims = self.dims()?;

        let _has_closing_parenthesis = match self.next_token() {
            Some(TokenSpan {
                token: Token::RParen,
                span: _,
            }) => true,
            _ => {
                return Err(ParserError::Expected(
                    "Expected Closing Parenthesis ')' at the end of an array".to_string(),
                    vec![Token::RParen],
                    self.peek_token(),
                ))
            }
        };

        Ok(Expr::VarDeclarationExpr(
            Box::new(Expr::Identifier(identifier.token)),
            Some(Box::new(dims)),
        ))
    }

    fn dims(&mut self) -> ParserResult<Expr> {
        let mut dimensions = vec![];

        loop {
            let dim = self.dim()?;
            dimensions.push(dim);

            match self.peek_token() {
                Some(TokenSpan {
                    token: Token::Comma,
                    span: _,
                }) => {
                    self.next_token();
                    continue;
                }
                _ => break,
            }
        }

        if dimensions.len() == 0 {
            Err(ParserError::Expected(
                "Expected an Identifier or Integer, cannot define an array with 0 dimensions"
                    .to_string(),
                vec![
                    Token::Integer(0),
                    Token::Word(Word::Identifier(Identifier("".to_string()))),
                ],
                self.peek_token(),
            ))
        } else {
            let dimensions = {
                let mut x = vec![];
                for dim in dimensions {
                    x.push(Box::new(dim))
                }
                x
            };
            Ok(Expr::DimensionsExpr(dimensions))
        }
    }

    fn dim(&mut self) -> ParserResult<Expr> {
        let token = self.next_token();

        Ok(Expr::DimensionExpr(Box::new(match token {
            Some(TokenSpan { token, span }) => match token {
                Token::Integer(_) => Expr::Constant(token),
                Token::Word(Word::Identifier(_)) => Expr::Identifier(token),
                _ => {
                    return Err(ParserError::Expected(
                        "A Dimension can only be of type Integer or Identifier".to_string(),
                        vec![
                            Token::Integer(0),
                            Token::Word(Word::Identifier(Identifier("".to_string()))),
                        ],
                        Some(TokenSpan { token, span }),
                    ))
                }
            },
            None => {
                return Err(ParserError::Expected(
                    "A Dimension can only be of type Integer or Identifier".to_string(),
                    vec![
                        Token::Integer(0),
                        Token::Word(Word::Identifier(Identifier("".to_string()))),
                    ],
                    None,
                ))
            }
        })))
    }

    fn values(&mut self) -> ParserResult<Expr> {
        let mut values = vec![];

        loop {
            let value = self.value()?;

            values.push(Box::new(value));

            match self.peek_token() {
                Some(TokenSpan {
                    token: Token::Comma,
                    span: _,
                }) => {
                    self.next_token();
                    continue;
                }
                _ => break,
            }
        }

        Ok(Expr::ValsExpr(values))
    }

    fn value(&mut self) -> ParserResult<Expr> {
        todo!();
    }

    fn repeat(&mut self) -> ParserResult<Expr> {
        match self.peek_token() {
            Some(TokenSpan {
                token: Token::Integer(integer),
                span: _,
            }) => {
                self.next_token();
                Ok(Expr::RepeatExpr(Box::new(Expr::Constant(Token::Integer(
                    integer,
                )))))
            }
            _ => Ok(Expr::EmptyExpr),
        }
    }

    fn next_token(&mut self) -> Option<TokenSpan> {
        self.current_token += 1;
        if self.tokens.len() > self.current_token - 1 {
            Some(self.tokens[self.current_token - 1].clone())
        } else {
            None
        }
    }

    fn prev_token(&mut self) -> Option<TokenSpan> {
        self.current_token = (self.current_token - 1).max(0);

        if self.tokens.len() > self.current_token {
            Some(self.tokens[self.current_token].clone())
        } else {
            None
        }
    }

    fn peek_token(&self) -> Option<TokenSpan> {
        if self.tokens.len() > self.current_token {
            Some(self.tokens[self.current_token].clone())
        } else {
            None
        }
    }

    /// # Expect
    ///
    /// Uses next_token if it actually is the expected token
    ///
    /// Does not use next_token if it is not expected
    fn expect(&mut self, expected: Token) -> Option<TokenSpan> {
        match self.peek_token() {
            Some(t) if t.token == expected => self.next_token(),
            _ => None,
        }
    }
}

#[test]
fn delcarations_with_fields_and_types() {
    let input = r#"record integer _first_ endrec _second_ logical _third_"#.to_string();
    let mut lexer = Lexer::new(input.chars().peekable());
    let tokens = lexer.lex().unwrap();

    let mut parser = Parser::new(tokens);

    let declarations = parser.declarations().unwrap();

    assert_eq!(
        declarations,
        Expr::DeclarationsExpr(vec![
            Box::new(Expr::DeclarationExpr(Box::new(
                Expr::DeclarationExprRecord(
                    Box::new(Expr::FieldsExpr(vec![Box::new(Expr::FieldExpr(Box::new(
                        Expr::FieldExprType(
                            Box::new(Expr::TypeExpr(Box::new(Expr::Keyword(Token::Word(
                                Word::Keyword(Keyword::Integer)
                            ))))),
                            Box::new(Expr::VarsExpr(vec![Box::new(Expr::VarDeclarationExpr(
                                Box::new(Expr::Identifier(Token::Word(Word::Identifier(
                                    Identifier("_first_".to_string())
                                )))),
                                None
                            ))]))
                        )
                    )))])),
                    Box::new(Expr::VarsExpr(vec![Box::new(Expr::VarDeclarationExpr(
                        Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                            "_second_".to_string()
                        ))))),
                        None
                    ))]))
                )
            ))),
            Box::new(Expr::DeclarationExpr(Box::new(Expr::DeclarationExprType(
                Box::new(Expr::TypeExpr(Box::new(Expr::Keyword(Token::Word(
                    Word::Keyword(Keyword::Logical)
                ))))),
                Box::new(Expr::VarsExpr(vec![Box::new(Expr::VarDeclarationExpr(
                    Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                        "_third_".to_string()
                    ))))),
                    None
                ))]))
            ))))
        ])
    );
}

#[test]
fn field_with_record() {
    let input = r#"record integer _hello_ endrec _hello_2_"#.to_string();

    let mut lexer = Lexer::new(input.chars().peekable());
    let tokens = lexer.lex().unwrap();

    let mut parser = Parser::new(tokens);

    let fields = parser.field().unwrap();

    assert_eq!(
        fields,
        Expr::FieldExprRecord(
            Box::new(Expr::FieldsExpr(vec![Box::new(Expr::FieldExpr(Box::new(
                Expr::FieldExprType(
                    Box::new(Expr::TypeExpr(Box::new(Expr::Keyword(Token::Word(
                        Word::Keyword(Keyword::Integer)
                    ))))),
                    Box::new(Expr::VarsExpr(vec![Box::new(Expr::VarDeclarationExpr(
                        Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                            "_hello_".to_string()
                        ))))),
                        None
                    ))]))
                )
            )))])),
            Box::new(Expr::VarsExpr(vec![Box::new(Expr::VarDeclarationExpr(
                Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                    "_hello_2_".to_string()
                ))))),
                None
            ))]))
        )
    )
}

#[test]
fn fields_with_type_only() {
    let input = r#"integer _hello_, _one_, _two_(_hello_, 1)
                          real _hello_, _one_, _two_(_hello_, 1)"#
        .to_string();

    let mut lexer = Lexer::new(input.chars().peekable());
    let tokens = lexer.lex().unwrap();

    let mut parser = Parser::new(tokens);

    let fields = parser.fields().unwrap();

    assert_eq!(
        fields,
        Expr::FieldsExpr(vec![
            Box::new(Expr::FieldExpr(Box::new(Expr::FieldExprType(
                Box::new(Expr::TypeExpr(Box::new(Expr::Keyword(Token::Word(
                    Word::Keyword(Keyword::Integer)
                ))))),
                Box::new(Expr::VarsExpr(vec![
                    Box::new(Expr::VarDeclarationExpr(
                        Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                            "_hello_".to_string()
                        ))))),
                        None
                    )),
                    Box::new(Expr::VarDeclarationExpr(
                        Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                            "_one_".to_string()
                        ))))),
                        None
                    )),
                    Box::new(Expr::VarDeclarationExpr(
                        Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                            "_two_".to_string()
                        ))))),
                        Some(Box::new(Expr::DimensionsExpr(vec![
                            Box::new(Expr::DimensionExpr(Box::new(Expr::Identifier(
                                Token::Word(Word::Identifier(Identifier("_hello_".to_string())))
                            )))),
                            Box::new(Expr::DimensionExpr(Box::new(Expr::Constant(
                                Token::Integer(1)
                            )))),
                        ])))
                    ))
                ]))
            )))),
            Box::new(Expr::FieldExpr(Box::new(Expr::FieldExprType(
                Box::new(Expr::TypeExpr(Box::new(Expr::Keyword(Token::Word(
                    Word::Keyword(Keyword::Real)
                ))))),
                Box::new(Expr::VarsExpr(vec![
                    Box::new(Expr::VarDeclarationExpr(
                        Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                            "_hello_".to_string()
                        ))))),
                        None
                    )),
                    Box::new(Expr::VarDeclarationExpr(
                        Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                            "_one_".to_string()
                        ))))),
                        None
                    )),
                    Box::new(Expr::VarDeclarationExpr(
                        Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                            "_two_".to_string()
                        ))))),
                        Some(Box::new(Expr::DimensionsExpr(vec![
                            Box::new(Expr::DimensionExpr(Box::new(Expr::Identifier(
                                Token::Word(Word::Identifier(Identifier("_hello_".to_string())))
                            )))),
                            Box::new(Expr::DimensionExpr(Box::new(Expr::Constant(
                                Token::Integer(1)
                            )))),
                        ])))
                    ))
                ]))
            ))))
        ])
    )
}

#[test]
fn field_type() {
    declaration_type();
}

#[test]
fn declaration_type() {
    let input = "integer _hello_, _one_, _two_(_hello_, 1)".to_string();

    let mut lexer = Lexer::new(input.chars().peekable());
    let tokens = lexer.lex().unwrap();

    let mut parser = Parser::new(tokens);

    let declaration = parser.declaration().unwrap();

    assert_eq!(
        declaration,
        Expr::DeclarationExprType(
            Box::new(Expr::TypeExpr(Box::new(Expr::Keyword(Token::Word(
                Word::Keyword(Keyword::Integer)
            ))))),
            Box::new(Expr::VarsExpr(vec![
                Box::new(Expr::VarDeclarationExpr(
                    Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                        "_hello_".to_string()
                    ))))),
                    None
                )),
                Box::new(Expr::VarDeclarationExpr(
                    Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                        "_one_".to_string()
                    ))))),
                    None
                )),
                Box::new(Expr::VarDeclarationExpr(
                    Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                        "_two_".to_string()
                    ))))),
                    Some(Box::new(Expr::DimensionsExpr(vec![
                        Box::new(Expr::DimensionExpr(Box::new(Expr::Identifier(
                            Token::Word(Word::Identifier(Identifier("_hello_".to_string())))
                        )))),
                        Box::new(Expr::DimensionExpr(Box::new(Expr::Constant(
                            Token::Integer(1)
                        )))),
                    ])))
                ))
            ]))
        )
    )
}

#[test]
fn types() {
    let input = "integer".to_string();

    let mut lexer = Lexer::new(input.chars().peekable());
    let tokens = lexer.lex().unwrap();

    let mut parser = Parser::new(tokens);

    let type_expr = parser.type_expr().unwrap();

    assert_eq!(
        type_expr,
        Expr::TypeExpr(Box::new(Expr::Keyword(Token::Word(Word::Keyword(
            Keyword::Integer
        )))))
    )
}

#[test]
fn vars() {
    let input = "_hello_, _one_, _two_(_hello_, 1)".to_string();

    let mut lexer = Lexer::new(input.chars().peekable());
    let tokens = lexer.lex().unwrap();

    let mut parser = Parser::new(tokens);

    let vars = parser.vars().unwrap();

    assert_eq!(
        vars,
        Expr::VarsExpr(vec![
            Box::new(Expr::VarDeclarationExpr(
                Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                    "_hello_".to_string()
                ))))),
                None
            )),
            Box::new(Expr::VarDeclarationExpr(
                Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                    "_one_".to_string()
                ))))),
                None
            )),
            Box::new(Expr::VarDeclarationExpr(
                Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                    "_two_".to_string()
                ))))),
                Some(Box::new(Expr::DimensionsExpr(vec![
                    Box::new(Expr::DimensionExpr(Box::new(Expr::Identifier(
                        Token::Word(Word::Identifier(Identifier("_hello_".to_string())))
                    )))),
                    Box::new(Expr::DimensionExpr(Box::new(Expr::Constant(
                        Token::Integer(1)
                    )))),
                ])))
            ))
        ])
    )
}

#[test]
fn undef_variable_array() {
    let input = "_hello_(_hello_, 1, 2)".to_ascii_lowercase();

    let mut lexer = Lexer::new(input.chars().peekable());
    let tokens = lexer.lex().unwrap();

    let mut parser = Parser::new(tokens);

    let undef_variable = parser.undef_variable().unwrap();

    assert_eq!(
        undef_variable,
        Expr::VarDeclarationExpr(
            Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                "_hello_".to_string()
            ))))),
            Some(Box::new(Expr::DimensionsExpr(vec![
                Box::new(Expr::DimensionExpr(Box::new(Expr::Identifier(
                    Token::Word(Word::Identifier(Identifier("_hello_".to_string())))
                )))),
                Box::new(Expr::DimensionExpr(Box::new(Expr::Constant(
                    Token::Integer(1)
                )))),
                Box::new(Expr::DimensionExpr(Box::new(Expr::Constant(
                    Token::Integer(2)
                ))))
            ])))
        )
    );
}

#[test]
fn undef_variable_single() {
    let input = "_hello_".to_ascii_lowercase();

    let mut lexer = Lexer::new(input.chars().peekable());
    let tokens = lexer.lex().unwrap();

    let mut parser = Parser::new(tokens);

    let undef_variable = parser.undef_variable().unwrap();

    assert_eq!(
        undef_variable,
        Expr::VarDeclarationExpr(
            Box::new(Expr::Identifier(Token::Word(Word::Identifier(Identifier(
                "_hello_".to_string()
            ))))),
            None
        )
    );
}

#[test]
fn dims() {
    let dim = "_hello_,1,2".to_string();
    let mut lexer = Lexer::new(dim.chars().peekable());
    let tokens = lexer.lex().unwrap();

    let mut parser = Parser::new(tokens);

    let dims = parser.dims().unwrap();
    assert_eq!(
        dims,
        Expr::DimensionsExpr(vec![
            Box::new(Expr::DimensionExpr(Box::new(Expr::Identifier(
                Token::Word(Word::Identifier(Identifier("_hello_".to_string())))
            )))),
            Box::new(Expr::DimensionExpr(Box::new(Expr::Constant(
                Token::Integer(1)
            )))),
            Box::new(Expr::DimensionExpr(Box::new(Expr::Constant(
                Token::Integer(2)
            ))))
        ])
    )
}

#[test]
fn dim_identifier() {
    let dim = "_hello_".to_string();
    let mut lexer = Lexer::new(dim.chars().peekable());
    let tokens = lexer.lex().unwrap();

    let mut parser = Parser::new(tokens);

    let dim = parser.dim().unwrap();

    assert_eq!(
        dim,
        Expr::DimensionExpr(Box::new(Expr::Identifier(Token::Word(Word::Identifier(
            Identifier("_hello_".to_string())
        )))))
    )
}

#[test]
fn dim_integer() {
    let dim = "1".to_string();
    let mut lexer = Lexer::new(dim.chars().peekable());
    let tokens = lexer.lex().unwrap();

    let mut parser = Parser::new(tokens);

    let dim = parser.dim().unwrap();
    assert_eq!(
        dim,
        Expr::DimensionExpr(Box::new(Expr::Constant(Token::Integer(1))))
    )
}

mod capturing;
mod context;
