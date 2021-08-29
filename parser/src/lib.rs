mod cursor;

use cursor::Cursor;
use lexer::{
    token::{Identifier, Word},
    token::{Keyword, Token},
    TokenSpan,
};

use colored::*;


use ptree::{TreeBuilder, print_tree};

trait Treeable { 
    fn add_to_tree(&self, tree: &mut TreeBuilder);
}

#[derive(Debug, Clone)]
pub enum ParseError {
    InvalidState,
    Expected(String, Vec<Token>, Option<TokenSpan>),
    EOF,
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone)]
pub struct Program {
    body: Body,
    subprograms: Vec<Subprogram>,
}

#[derive(Debug, Clone)]
pub struct Body {
    declarations: Declarations,
    statements: Statements,
}

#[derive(Debug, Clone)]
pub struct Declarations(Vec<Declaration>);

#[derive(Debug, Clone)]
pub enum Declaration {
    Basic(BasicVarDeclaration),
    Record(RecordVarDeclaration),
}

#[derive(Debug, Clone)]
pub struct RecordVarDeclaration {
    fields: Fields,
    vars: VarDeclarations,
}

impl RecordVarDeclaration {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        cursor.expect(Token::Word(Word::Keyword(Keyword::Record)))?;

        let fields = Fields::parse(cursor)?;

        cursor.expect(Token::Word(Word::Keyword(Keyword::Endrec)))?;

        let vars = VarDeclarations::parse(cursor)?;

        Ok(Self { fields, vars })
    }
}

impl Treeable for RecordVarDeclaration { 
    fn add_to_tree(&self, tree: &mut TreeBuilder) {
        tree.begin_child("RecordVarDeclaration".bright_blue().to_string());
        
        self.fields.add_to_tree(tree);

        tree.end_child();
    }
}

#[test]
fn test_record_var_declaration() { 
    let mut cursor = to_cursor("record integer _hello_, _array_(1, 2, _ident_) real _real_ident_ endrec _hello_2_, _array_2_(1, 2, _ident_2_)");

    let record = RecordVarDeclaration::parse(&mut cursor).unwrap();

    let mut tree = TreeBuilder::new("Root".magenta().to_string());
    
    record.add_to_tree(&mut tree);

    print_tree(&tree.build());
    
    assert_eq!(record.fields.0.len(), 2);
    assert_eq!(record.vars.0.len(), 2);
}

#[derive(Debug, Clone)]
pub struct VarDeclarations(Vec<VarDeclaration>);

impl VarDeclarations {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        let mut declarations = vec![];

        loop {
            let declaration = VarDeclaration::parse(cursor)?;

            declarations.push(declaration);

            match cursor.peek() {
                Some(TokenSpan {
                    token: Token::Comma,
                    ..
                }) => {
                    cursor.next();
                    continue;
                }
                _ => break,
            }
        }

        Ok(VarDeclarations(declarations))
    }
}

#[test]
fn test_var_declarations_multiple() {
    let mut cursor = to_cursor("_single_, _array_(1, 2, _ident_)");

    let declarations = VarDeclarations::parse(&mut cursor).unwrap();

    assert_eq!(declarations.0.len(), 2);

    let first = declarations.0[0].clone();

    if let VarDeclaration::Single(first) = first {
        assert_eq!(
            first.0.token,
            Token::Word(Word::Identifier(Identifier("_single_".to_string())))
        );
    } else {
        panic!("Expected VarDeclaration::Single, got {:?}", first);
    }

    let second = declarations.0[1].clone();
    if let VarDeclaration::Array {
        id: ID(TokenSpan { token, .. }),
        dimensions,
    } = second
    {
        assert_eq!(
            token,
            Token::Word(Word::Identifier(Identifier("_array_".to_string())))
        );
        assert_eq!(dimensions.0.len(), 3);
    } else {
        panic!("Expected VarDeclaration::Array, got {:?}", second);
    }
}

#[derive(Debug, Clone)]
pub enum VarDeclaration {
    Single(ID),
    Array { id: ID, dimensions: Dimensions },
}

impl VarDeclaration {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        let id = match cursor.next() {
            Some(TokenSpan {
                token: Token::Word(Word::Identifier(ident)),
                span,
            }) => TokenSpan {
                token: Token::Word(Word::Identifier(ident)),
                span,
            },
            token => {
                return Err(ParseError::Expected(
                    "Expected an Identifier".to_string(),
                    vec![Token::Word(Word::Identifier(Identifier(
                        "identifier".to_string(),
                    )))],
                    token,
                ))
            }
        };

        match cursor.peek() {
            Some(TokenSpan {
                token: Token::LParen,
                ..
            }) => {
                cursor.next();
                let dims = Dimensions::parse(cursor)?;
                cursor.expect(Token::RParen)?;

                Ok(Self::Array {
                    id: ID(id),
                    dimensions: dims,
                })
            }
            _ => Ok(Self::Single(ID(id))),
        }
    }
}

#[test]
fn test_var_declaration_array() {
    let mut cursor = to_cursor("_id_(1, 2, _dim_)");

    let declaration = VarDeclaration::parse(&mut cursor).unwrap();

    if let VarDeclaration::Array { id, dimensions } = declaration {
        assert_eq!(dimensions.0.len(), 3);
        assert_eq!(
            id.0.token,
            Token::Word(Word::Identifier(Identifier("_id_".to_string())))
        );
    } else {
        panic!("Expected VarDeclaration::Array");
    }
}

#[test]
fn test_var_declaration_single() {
    let mut cursor = to_cursor("_id_");

    let declaration = VarDeclaration::parse(&mut cursor).unwrap();

    if let VarDeclaration::Single(declaration) = declaration {
        assert_eq!(
            declaration.0.token,
            Token::Word(Word::Identifier(Identifier("_id_".to_string())))
        )
    } else {
        panic!("Expected declaration of type VarDeclaration::Single");
    }
}

#[derive(Debug, Clone)]
pub struct Dimensions(Vec<Dimention>);

impl Dimensions {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        let mut dimensions = vec![];

        loop {
            let dim = Dimention::parse(cursor)?;
            dimensions.push(dim);

            match cursor.peek() {
                Some(TokenSpan {
                    token: Token::Comma,
                    ..
                }) => {
                    cursor.next();
                    continue;
                }
                _ => break,
            }
        }

        Ok(Dimensions(dimensions))
    }
}

#[test]
fn test_dimensions() {
    let mut cursor = to_cursor("1,_hello_");

    let dimention = Dimensions::parse(&mut cursor).unwrap();

    assert_eq!(dimention.0.len(), 2);
}

#[derive(Debug, Clone)]
pub enum Dimention {
    Integer(TokenSpan),
    ID(ID),
}

impl Dimention {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        Ok(match cursor.next() {
            Some(TokenSpan {
                token: Token::Integer(int),
                span,
            }) => Dimention::Integer(TokenSpan {
                token: Token::Integer(int),
                span,
            }),
            Some(TokenSpan {
                token: Token::Word(Word::Identifier(id)),
                span,
            }) => Dimention::ID(ID(TokenSpan {
                token: Token::Word(Word::Identifier(id)),
                span,
            })),
            Some(t) => {
                return Err(ParseError::Expected(
                    "A Dimension can only be of type Integer or Identifier".to_string(),
                    vec![
                        Token::Integer(0),
                        Token::Word(Word::Identifier(Identifier("".to_string()))),
                    ],
                    Some(t),
                ))
            }
            _ => return Err(ParseError::EOF),
        })
    }
}

#[derive(Debug, Clone)]
pub struct Fields(Vec<Field>);

impl Treeable for Fields { 
    fn add_to_tree(&self, tree: &mut TreeBuilder) {
        tree.begin_child(format!("{} ({})", "Fields".bright_blue(), self.0.len().to_string().yellow()).black().to_string());
    }
}

impl Fields {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        let mut fields = vec![];

        while let Ok(field) = Field::parse(cursor) {
            fields.push(field);
        }

        assert!(fields.len() > 0);

        Ok(Fields(fields))
    }
}

#[derive(Debug, Clone)]
pub enum Field {
    Record(RecordFieldDeclaration),
    Basic(BasicFieldDeclaration),
}

impl Field {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        match cursor.peek() {
            Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Record)),
                ..
            }) => { 
                let record = RecordFieldDeclaration::parse(cursor)?;
                return Ok(Field::Record(record))
            },
            _ => {
                let field_type = DeclarationType::parse(cursor)?;
                let vars = VarDeclarations::parse(cursor)?;

                return Ok(Field::Basic(BasicFieldDeclaration {
                    declaration_type: field_type,
                    vars,
                }));
            }
        }
    }
}



#[test]
fn basic_field_test() {
    let mut cursor = to_cursor("integer _hello_, _array_(1, 2, _ident_)");

    let field = Field::parse(&mut cursor).unwrap();

    if let Field::Basic(field) = field {
        assert_eq!(
            field.declaration_type.0.token,
            Token::Word(Word::Keyword(Keyword::Integer))
        );
        assert_eq!(field.vars.0.len(), 2);
    } else {
        panic!("Expected Field::Basic");
    }
}

pub type BasicFieldDeclaration = BasicVarDeclaration;
pub type RecordFieldDeclaration = RecordVarDeclaration;

#[cfg(test)]
fn to_cursor(input: &str) -> Cursor {
    use lexer::Lexer;

    let input = input.to_string();
    let mut lexer = Lexer::new(input.chars().peekable());
    let lex = lexer.lex().unwrap();

    let cursor = Cursor::new(lex);
    cursor
}

#[test]
fn test_dimention() {
    let mut cursor = to_cursor("1");

    let dimention = Dimention::parse(&mut cursor).unwrap();

    if let Dimention::Integer(TokenSpan { token, .. }) = dimention {
        assert_eq!(token, Token::Integer(1))
    } else {
        panic!("Dimention is not of type Integer, {:?}", dimention);
    }
}

#[test]
fn test_dimention_ident() {
    let mut cursor = to_cursor("_hello_");

    let dimention = Dimention::parse(&mut cursor).unwrap();

    if let Dimention::ID(ID(TokenSpan { token, .. })) = dimention {
        assert_eq!(
            token,
            Token::Word(Word::Identifier(Identifier("_hello_".to_string())))
        )
    } else {
        panic!("Dimention is not of type Integer, {:?}", dimention);
    }
}

#[derive(Debug, Clone)]
pub struct BasicVarDeclaration {
    declaration_type: DeclarationType,
    vars: VarDeclarations,
}

impl BasicVarDeclaration {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        let type_ = DeclarationType::parse(cursor)?;
        let vars = VarDeclarations::parse(cursor)?;

        Ok(Self {
            declaration_type: type_,
            vars,
        })
    }
}

#[test]
fn test_basic_var_declaration() {
    let mut cursor = to_cursor("integer _hello_(1, 2, _ident_)");

    let declaration = BasicVarDeclaration::parse(&mut cursor).unwrap();

    assert_eq!(
        declaration.declaration_type.0.token,
        Token::Word(Word::Keyword(Keyword::Integer))
    );

    assert_eq!(declaration.vars.0.len(), 1);
}

#[derive(Debug, Clone)]
pub struct DeclarationType(TokenSpan);

impl DeclarationType {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        match cursor.peek() {
            Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Integer)),
                ..
            })
            | Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Real)),
                ..
            })
            | Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Character)),
                ..
            })
            | Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Logical)),
                ..
            }) => {
                let token = cursor.next().unwrap();
                return Ok(Self(token));
            }
            token => {
                return Err(ParseError::Expected(
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
}

#[test]
fn test_declaration_type() {
    let mut cursor = to_cursor("integer");

    let decl_type = DeclarationType::parse(&mut cursor).unwrap();
    assert_eq!(
        decl_type.0.token,
        Token::Word(Word::Keyword(Keyword::Integer))
    )
}

#[derive(Debug, Clone)]
pub struct Statements(Vec<Statement>);

#[derive(Debug, Clone)]
pub enum Statement {}

#[derive(Debug, Clone)]
pub struct Subprogram;

#[derive(Debug, Clone)]
pub struct ID(TokenSpan);
