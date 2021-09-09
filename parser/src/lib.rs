pub mod cursor;

use serde::{Deserialize, Serialize};

use cursor::Cursor;
use lexer::{TokenSpan, token::{AssignOp, BinOp, Identifier, Word}, token::{Keyword, Token}};

use colored::*;

use ptree::{print_tree, TreeBuilder};

trait Treeable {
    fn add_to_tree(&self, tree: &mut TreeBuilder);
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum ParseError {
    InvalidState,
    Expected(String, Vec<Token>, Option<TokenSpan>),
    EOF,
    EndIf,
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Program {
    pub body: Body,
    pub subprograms: Vec<Subprogram>,
}

impl Program {
    pub fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        let body = Body::parse(cursor)?;

        Ok(Self {
            body,
            subprograms: vec![],
        })
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Body {
    pub declarations: Declarations,
    pub statements: StatementList,
}

impl Body {
    pub fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        let declarations = Declarations::parse(cursor)?;
        let statements = StatementList::parse(cursor)?;

        Ok(Self {
            declarations,
            statements,
        })
    }

    pub fn parse_ignore_error_if<Function: Fn(Option<TokenSpan>) -> bool>(cursor: &mut Cursor, f: Function) -> ParseResult<Self> {
        let declarations = Declarations::parse(cursor)?;
        let statements = StatementList::parse_ignore_error_if(cursor, f)?;

        Ok(Self {
            declarations,
            statements,
        })
    }
}

#[test]
fn body_test() {
    let mut cursor = to_cursor(
        r#"   
                                integer hello 
                                write "hello"
                           "#,
    );

    let program = Program::parse(&mut cursor).unwrap();

    println!("{:?}", program);
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Declarations(pub Vec<Declaration>);

impl Treeable for Declarations {
    fn add_to_tree(&self, tree: &mut TreeBuilder) {
        tree.begin_child(
            format!(
                "{} ({})",
                "Declarations".bright_blue(),
                self.0.len().to_string().yellow()
            )
            .bright_black()
            .to_string(),
        );

        for declaration in &self.0 {
            declaration.add_to_tree(tree);
        }

        tree.end_child();
    }
}

impl Declarations {
    pub fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        let mut declarations = vec![];

        loop {
            match Declaration::parse(cursor)? {
                Declaration::Empty => break,
                declaration => declarations.push(declaration),
            }
        }

        Ok(Self(declarations))
    }
}

impl Treeable for Declaration {
    fn add_to_tree(&self, tree: &mut TreeBuilder) {
        match self {
            Declaration::Basic(basic) => {
                tree.begin_child("Declaration::Basic".bright_blue().to_string());
                basic.add_to_tree(tree);
            }
            Declaration::Record(record) => {
                tree.begin_child("Declaration::Record".bright_blue().to_string());
                record.add_to_tree(tree);
            }
            Self::Empty => unreachable!(),
        }

        tree.end_child();
    }
}

#[test]
fn test_declarations() {
    let mut cursor = to_cursor(
        r#"
        record record record integer _3rd_record_, _3rd_var_, _3rd_array_(1, 2, _3rd_ident_) endrec _3rd_endrec_ endrec _2nd_endrec_ endrec _1st_endrec_
        integer hello_world
        character my_string(5)
        "#,
    );

    let declarations = Declarations::parse(&mut cursor).unwrap();

    let mut tree = TreeBuilder::new("Root".yellow().to_string());
    declarations.add_to_tree(&mut tree);

    print_tree(&tree.build()).unwrap();
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum Declaration {
    Basic(BasicVarDeclaration),
    Record(RecordVarDeclaration),
    Empty,
}

impl Declaration {
    pub fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        match cursor.peek() {
            Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Record)),
                ..
            }) => Ok(Declaration::Record(RecordVarDeclaration::parse(cursor)?)),
            Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Data)),
                ..
            }) => todo!(),
            _ => match DeclarationType::parse(cursor) {
                Ok(type_declaration) => {
                    let vars = VarDeclarations::parse(cursor)?;

                    Ok(Declaration::Basic(BasicVarDeclaration {
                        declaration_type: type_declaration,
                        vars,
                    }))
                }
                _ => Ok(Declaration::Empty),
            },
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct RecordVarDeclaration {
    pub fields: Fields,
    pub vars: VarDeclarations,
}

impl RecordVarDeclaration {
    pub fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
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
        self.vars.add_to_tree(tree);

        tree.end_child();
    }
}

#[test]
fn test_record_var_declaration() {
    let mut cursor = to_cursor("record integer _hello_, _array_(1, 2, _ident_) real _real_ident_ endrec _hello_2_, _array_2_(1, 2, _ident_2_)");

    let record = RecordVarDeclaration::parse(&mut cursor).unwrap();

    let mut tree = TreeBuilder::new("Root".magenta().to_string());

    record.add_to_tree(&mut tree);

    print_tree(&tree.build()).unwrap();

    assert_eq!(record.fields.0.len(), 2);
    assert_eq!(record.vars.0.len(), 2);
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct VarDeclarations(pub Vec<VarDeclaration>);

impl Treeable for VarDeclarations {
    fn add_to_tree(&self, tree: &mut TreeBuilder) {
        tree.begin_child("VarDeclarations".bright_blue().to_string());

        for v in &self.0 {
            v.add_to_tree(tree);
        }

        tree.end_child();
    }
}

impl VarDeclarations {
    pub fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
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

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum VarDeclaration {
    Single(ID),
    Array { id: ID, dimensions: Dimensions },
}

impl Treeable for VarDeclaration {
    fn add_to_tree(&self, tree: &mut TreeBuilder) {
        match self {
            VarDeclaration::Single(single) => {
                tree.begin_child("VarDeclaration::1D".bright_blue().to_string());
                single.add_to_tree(tree);
            }
            VarDeclaration::Array { id, dimensions } => {
                tree.begin_child(
                    format!("VarDeclaration::{}D", dimensions.0.len())
                        .bright_blue()
                        .to_string(),
                );
                id.add_to_tree(tree);
                dimensions.add_to_tree(tree);
            }
        }
        tree.end_child();
    }
}

impl VarDeclaration {
    pub fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
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

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Dimensions(pub Vec<Dimension>);

impl Treeable for Dimensions {
    fn add_to_tree(&self, tree: &mut TreeBuilder) {
        tree.begin_child(
            format!(
                "{} ({})",
                "Dimensions".bright_blue(),
                self.0.len().to_string().yellow()
            )
            .bright_black()
            .to_string(),
        );

        for v in &self.0 {
            v.add_to_tree(tree);
        }

        tree.end_child();
    }
}

impl Dimensions {
    pub fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        let mut dimensions = vec![];

        loop {
            let dim = Dimension::parse(cursor)?;
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

    let dimension = Dimensions::parse(&mut cursor).unwrap();

    assert_eq!(dimension.0.len(), 2);
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum Dimension {
    Integer(TokenSpan),
    ID(ID),
}

impl Treeable for Dimension {
    fn add_to_tree(&self, tree: &mut TreeBuilder) {
        tree.begin_child("Dimension".bright_blue().to_string());
        match self {
            Dimension::ID(id) => {
                id.add_to_tree(tree);
            }
            Dimension::Integer(TokenSpan { token, .. }) => {
                tree.add_empty_child(
                    format!("{} ({})", "Integer".green(), token.value().yellow())
                        .bright_black()
                        .to_string(),
                );
            }
        }
        tree.end_child();
    }
}

impl Dimension {
    pub fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        Ok(match cursor.next() {
            Some(TokenSpan {
                token: Token::Integer(int),
                span,
            }) => Dimension::Integer(TokenSpan {
                token: Token::Integer(int),
                span,
            }),
            Some(TokenSpan {
                token: Token::Word(Word::Identifier(id)),
                span,
            }) => Dimension::ID(ID(TokenSpan {
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

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Fields(Vec<Field>);

impl Treeable for Fields {
    fn add_to_tree(&self, tree: &mut TreeBuilder) {
        tree.begin_child(
            format!(
                "{} ({})",
                "Fields".bright_blue(),
                self.0.len().to_string().yellow()
            )
            .bright_black()
            .to_string(),
        );

        for field in &self.0 {
            field.add_to_tree(tree);
        }

        tree.end_child();
    }
}

impl Fields {
    pub fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        let mut fields = vec![];

        while let Ok(field) = Field::parse(cursor) {
            fields.push(field);
        }

        assert!(fields.len() > 0);

        Ok(Fields(fields))
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum Field {
    Record(RecordFieldDeclaration),
    Basic(BasicFieldDeclaration),
}

impl Treeable for Field {
    fn add_to_tree(&self, tree: &mut TreeBuilder) {
        match self {
            Field::Basic(basic) => {
                tree.begin_child("Field::Basic".bright_blue().to_string());

                basic.add_to_tree(tree);

                tree.end_child();
            }
            Field::Record(record) => {
                tree.begin_child("Field::Record".bright_blue().to_string());

                record.add_to_tree(tree);

                tree.end_child();
            }
        };
    }
}

impl Field {
    pub fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        match cursor.peek() {
            Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Record)),
                ..
            }) => {
                let record = RecordFieldDeclaration::parse(cursor)?;
                return Ok(Field::Record(record));
            }
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
fn test_dimension() {
    let mut cursor = to_cursor("1");

    let dimension = Dimension::parse(&mut cursor).unwrap();

    if let Dimension::Integer(TokenSpan { token, .. }) = dimension {
        assert_eq!(token, Token::Integer(1))
    } else {
        panic!("Dimension is not of type Integer, {:?}", dimension);
    }
}

#[test]
fn test_dimension_ident() {
    let mut cursor = to_cursor("_hello_");

    let dimension = Dimension::parse(&mut cursor).unwrap();

    if let Dimension::ID(ID(TokenSpan { token, .. })) = dimension {
        assert_eq!(
            token,
            Token::Word(Word::Identifier(Identifier("_hello_".to_string())))
        )
    } else {
        panic!("Dimension is not of type Integer, {:?}", dimension);
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct BasicVarDeclaration {
    pub declaration_type: DeclarationType,
    pub vars: VarDeclarations,
}

impl Treeable for BasicVarDeclaration {
    fn add_to_tree(&self, tree: &mut TreeBuilder) {
        tree.begin_child("BasicVarDeclaration".bright_blue().to_string());
        self.declaration_type.add_to_tree(tree);
        self.vars.add_to_tree(tree);
        tree.end_child();
    }
}

impl BasicVarDeclaration {
    pub fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
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

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct DeclarationType(pub TokenSpan);

impl Treeable for DeclarationType {
    fn add_to_tree(&self, tree: &mut TreeBuilder) {
        tree.add_empty_child(
            format!(
                "{} ({})",
                "Type".green(),
                self.0.token.value().yellow().to_string()
            )
            .bright_black()
            .to_string(),
        );
    }
}

impl DeclarationType {
    pub fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
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
            })
            | Some(TokenSpan { token: Token::Word(Word::Keyword(Keyword::String)), ..}) => {
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
                        Token::Word(Word::Keyword(Keyword::String
                        )),
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

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Subprogram;

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ID(pub TokenSpan);

impl Treeable for ID {
    fn add_to_tree(&self, tree: &mut TreeBuilder) {
        tree.add_empty_child(
            format!(
                "{} ({})",
                "Identifier".green(),
                self.0.token.value().to_string().yellow()
            )
            .bright_black()
            .to_string(),
        );
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct StatementList(pub Vec<LabeledStatement>);

impl StatementList {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        let mut statements = vec![];

        while let Ok(statement) = LabeledStatement::parse(cursor) {
            statements.push(statement);
        }

        Ok(StatementList(statements))
    }

    pub fn parse_ignore_error_if<Function: Fn(Option<TokenSpan>) -> bool> (cursor: &mut Cursor, f: Function) -> ParseResult<Self> {
        let mut statements = vec![];

        loop { 
            match LabeledStatement::parse(cursor) { 
                Ok(statement) => statements.push(statement),
                Err(ParseError::Expected(..)) if f(cursor.peek()) => break,
                Err(e) => return Err(e)
            }
        }

        Ok(StatementList(statements))
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct LabeledStatement {
    pub label: Option<TokenSpan>,
    pub statement: Statement,
}

impl LabeledStatement {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        match cursor.peek() {
            Some(TokenSpan {
                token: Token::Integer(iconst),
                span,
            }) => {
                cursor.next();
                Ok(LabeledStatement {
                    label: Some(TokenSpan {
                        token: Token::Integer(iconst),
                        span,
                    }),
                    statement: Statement::parse(cursor)?,
                })
            }
            Some(_) => Ok(LabeledStatement {
                label: None,
                statement: Statement::parse(cursor)?,
            }),
            _ => Err(ParseError::EOF),
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum Statement {
    Assignment(Assignment),
    Goto(),
    /// # If Statement
    ///
    /// See [IfStatement] as the definition of the `if_statement` was changed
    If(IfStatement),
    Loop(),
    FunctionCall(FunctionCall),
    Io(IoStatement),
    Continue(TokenSpan),
    Return(TokenSpan),
    Stop(TokenSpan),
}

impl Statement {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        match cursor.peek() {
            Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Write)),
                ..
            })
            | Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Read)),
                ..
            }) => Ok(Statement::Io(IoStatement::parse(cursor)?)),
            Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::If)),
                ..
            }) => Ok(Statement::If(IfStatement::parse(cursor)?)),
            Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Endif)),
                ..
            }) => Err(ParseError::EndIf),
            _ if cursor.check_if(1, Token::AssignOp(AssignOp::Assign)) => Ok(Statement::Assignment(Assignment::parse(cursor)?)),
            _ if cursor.check_if(1, Token::LParen) => Ok(Statement::FunctionCall(FunctionCall::parse(cursor)?)),
            Some(t) => todo!("Add support for Statement with {:?}", t.token),
            _ => todo!("Add error handling"),
        }
    }
}

#[test]
fn test_assignments_in_statements() { 
    let _ = env_logger::try_init();
    let mut cursor = to_cursor("hello = 1");

    let statement = Statement::parse(&mut cursor).unwrap();

    println!("{}", serde_json::to_string_pretty(&statement).unwrap());
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct FunctionCall { 
    pub identifier: ID,
    pub variables: FunctionVariableList,
    // return_type: DeclarationType,
    // body: Body
}

impl FunctionCall { 
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> { 
        let id = cursor.expect_ident()?;


        cursor.expect(Token::LParen)?;
        let vars = FunctionVariableList::parse(cursor)?;
        cursor.expect(Token::RParen)?;
        
        Ok(Self { 
                    identifier: ID(id),
                    variables: vars
                })
    }
}

#[test]
fn test_function_call() { 
    let mut cursor = to_cursor("hello(1, 2, 3)");

    let fn_call = FunctionCall::parse(&mut cursor).unwrap();

    println!("{}", serde_yaml::to_string(&fn_call).unwrap());
}

pub type FunctionVariableList = ExpressionList;

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum Constant {
    Integer(TokenSpan),
    Real(TokenSpan),
    Character(TokenSpan),
    Boolean(TokenSpan),
}

impl Constant {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        Ok(match cursor.next() {
            Some(TokenSpan {
                token: Token::Integer(int),
                span,
            }) => Constant::Integer(TokenSpan {
                token: Token::Integer(int),
                span,
            }),
            Some(TokenSpan {
                token: Token::Float(f),
                span,
            }) => Constant::Integer(TokenSpan {
                token: Token::Float(f),
                span,
            }),
            Some(TokenSpan {
                token: Token::Char(c),
                span,
            }) => Constant::Character(TokenSpan {
                token: Token::Char(c),
                span,
            }),
            Some(TokenSpan {
                token: Token::Boolean(b),
                span,
            }) => Constant::Boolean(TokenSpan {
                token: Token::Boolean(b),
                span,
            }),
            c => {
                return Err(ParseError::Expected(
                    "Expected a character, boolean, float or integer".to_string(),
                    vec![
                        Token::Boolean(true),
                        Token::Integer(0),
                        Token::Float(0f64),
                        Token::Char("".to_string()),
                    ],
                    c,
                ))
            }
        })
    }
}

/// # If Statement
///
/// Since the if statment had two definitions:
/// - if_statement -> it did not include THEN
///     - IF LPAREN expression RPAREN label COMMA label COMMA label
///     - IF LPAREN expression RPAREN simple_statement
///- branch_statement -> IF LPAREN expression RPAREN THEN body tail
///
/// The branch_statement and if_statement were so close, that it was decided to actually
/// Merge them into one, and to add some extra functionality, and to not go to the hussle of
/// having to seek through too many tokens in order to determine which one it actually isbecause
/// the beginning of both statements are identical
///
/// Result:
///
/// if_statement -> IF LPAREN expression RPAREN THEN body tail
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct IfStatement {
    pub expression: Expression,
    pub body: Body,
    pub tail: Option<Body>,
}

impl IfStatement {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        cursor.expect(Token::Word(Word::Keyword(Keyword::If)))?;
        cursor.expect(Token::LParen)?;

        let expression = Expression::parse(cursor)?;

        cursor.expect(Token::RParen)?;
        cursor.expect(Token::Word(Word::Keyword(Keyword::Then)))?;

        let body = Body::parse(cursor)?;

        cursor.expect(Token::Word(Word::Keyword(Keyword::Endif)))?;

        Ok(IfStatement {
            expression,
            body,
            tail: None,
        })
    }
}

#[test]
fn test_if_statement() {
    let mut cursor = to_cursor(
        r#"if ( .true. ) then integer x write "hello" if ( .false. ) then write "false" endif endif"#,
    );

    let if_statement = IfStatement::parse(&mut cursor).unwrap();

    println!("{}", serde_json::to_string(&if_statement).unwrap());
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum IoStatement {
    Read(),
    Write(WriteList),
}

impl IoStatement {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        match cursor.next() {
            Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Write)),
                span: _,
            }) => Ok(IoStatement::Write(WriteList::parse(cursor)?)),
            Some(TokenSpan {
                token: Token::Word(Word::Keyword(Keyword::Read)),
                span: _,
            }) => {
                todo!("Add support for IoStatement -> READ read_list")
            }
            _ => todo!("Add proper error support"),
        }
    }
}

#[test]
fn test_io_statement() { 
    let mut cursor = to_cursor("write x");

    let statement = IoStatement::parse(&mut cursor);

    println!("{:?}", statement);
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct WriteList(pub Vec<WriteItem>);

impl WriteList {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        let mut list = vec![];

        loop {
            let item = WriteItem::parse(cursor)?;

            list.push(item);

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

        Ok(WriteList(list))
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum WriteItem {
    Expression(Expression),
    /// Uhh.., not sure for what to name this
    ///
    ///  LPAREN write_list COMMA ID ASSIGN iter_space RPAREN
    Uhhh(),

    String(String),
}

impl WriteItem {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        match cursor.peek() {
            Some(TokenSpan {
                token: Token::String(string),
                span: _,
            }) => {
                cursor.next();
                Ok(Self::String(string))
            },
            Some(TokenSpan {
                token: Token::LParen,
                span: _,
            }) => {
                todo!("Add support for WriteItem -> LPAREN write_list COMMA ID ASSIGN iter_space RPAREN");
            }
            Some(TokenSpan { token: t, span: _ }) => {
                Ok(Self::Expression(Expression::parse(cursor)?))
            }
            next => Err(ParseError::Expected(
                "Expected either an expression, a string or left parenthesis".to_string(),
                vec![],
                next,
            )),
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ExpressionList(pub Vec<Expression>);

impl ExpressionList { 
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> { 
        let mut expressions = vec![];

        loop { 
            let e = Expression::parse(cursor);
            if let Ok(expression) =  e{
                expressions.push(expression);

                match cursor.peek() { 
                    Some(TokenSpan { token: Token::Comma, ..}) => { 
                        cursor.next();
                        continue;
                    },
                    _ => break
                }
            } else { 
                break;
            }
        }

        Ok(Self(expressions))
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum Expression {
    Empty,
    Or(),
    And(),
    /// > < >= <= etc
    Rel(),
    Add(),
    Mul(),
    Div(),
    Pow(),
    Not(),
    Variable(Variable),
    Constant(Constant),
    Expression(Box<Expression>),
    /// Added since just stirngs on their own could be considered an expression
    String(TokenSpan),
    Function(FunctionCall),
}

impl Expression {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
        if let Some(TokenSpan { token: Token::String(_), ..}) = cursor.peek() { 
            
            return Ok(Expression::String(cursor.next().unwrap()))
        }
        
        if let Ok(_) = Constant::parse(&mut cursor.clone()) {
            let constant = Constant::parse(cursor).unwrap();

            return Ok(Expression::Constant(constant));
        }
        
        if let Ok(_) = Variable::parse(&mut cursor.clone()) {
            let var = Variable::parse(cursor).unwrap();

            return Ok(Expression::Variable(var));
        }

        if let Ok(_) = FunctionCall::parse(&mut cursor.clone()) { 
            let func = FunctionCall::parse(cursor).unwrap();

            return Ok(Expression::Function(func));
        }

        Ok(Expression::Empty)
    }
}



#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Assignment { 
    pub variable: Variable,
    pub expression: Expression
}

impl Assignment { 
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> { 
        let variable = Variable::parse(cursor)?;
        
        cursor.expect(Token::AssignOp(AssignOp::Assign))?;
        
        let expression = Expression::parse(cursor)?;

        Ok(Self { 
                    variable,
                    expression
                })
    }
}

#[test]
fn test_assigments() { 
    let mut cursor = to_cursor(r#"id = "hello, world""#);

    let assigment = Assignment::parse(&mut cursor).unwrap();

    println!("{}", serde_json::to_string_pretty(&assigment).unwrap());
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Variables(Vec<Variable>);

impl Variables { 
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> { 
        let mut variables = vec![];

        loop { 
            let var = Variable::parse(cursor)?;

            variables.push(var);

            match cursor.peek() {
                Some(TokenSpan { token: Token::Colon, .. }) => { 
                    cursor.next();
                    continue;
                },
                _ => break
            }
        }

        Ok(Self(variables))
    }
}

#[test]
fn test_variables() { 
    let _ = env_logger::try_init();
    let mut cursor = to_cursor("one : two : three");

    let vars = Variables::parse(&mut cursor).unwrap();

    // println!("{}", serde_json::to_string_pretty(&vars).unwrap());
    assert_eq!(vars.0.len(), 3);
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum Variable { 
    Readable(ID),
    Callable(FunctionCall)
}

impl Variable {
    fn parse(cursor: &mut Cursor) -> ParseResult<Self> { 
        Ok(match cursor.check_if(1, Token::LParen) { 
            true => Variable::Callable(FunctionCall::parse(cursor)?),
            false => Variable::Readable(ID(cursor.expect_ident()?))
        })
    }
}

// pub struct Variable {
//     pub id: ID,
//     pub expressions: Option<ExpressionList>,
// }

// impl Variable {
//     fn parse(cursor: &mut Cursor) -> ParseResult<Self> {
//         let id = cursor.expect_ident()?;

//         println!("{:?}", cursor.peek());
//         let expressions = match cursor.peek() {
//             Some(TokenSpan {
//                 token: Token::LParen,
//                 span,
//             }) => todo!("Add support for LParen: variable -> ID LParen expressions RParen"),
//             _ => None,
//         };

//         Ok(Self {
//             id: ID(id),
//             expressions,
//         })
//     }
// }

#[test]
fn test_assignment_with_fn_calls() {
    let mut cursor = to_cursor("y = hello()");

    let assignment = Assignment::parse(&mut cursor).unwrap();
    println!("{:?}", assignment);
}
